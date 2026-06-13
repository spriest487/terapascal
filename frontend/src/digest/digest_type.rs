use crate::ast::Access;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::StructKind;
use crate::ast::Visibility;
use crate::codegen::SET_TAG_ITEM_TYPE_FIELD;
use crate::codegen::SET_TAG_MAX_FIELD;
use crate::codegen::SET_TAG_MIN_FIELD;
use crate::codegen::SET_TAG_NAME;
use crate::digest::DigestBuilder;
use crate::digest::DigestError;
use crate::digest::DigestResult;
use crate::digest::DigestWarning;
use crate::ir;
use crate::typ::ast::FieldDecl;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::MethodDeclSection;
use crate::typ::ast::StructDecl;
use crate::typ::ast::StructDeclSection;
use crate::typ::ast::StructMemberDecl;
use crate::typ::ast::VariantCase;
use crate::typ::ast::VariantCaseData;
use crate::typ::ast::VariantDecl;
use crate::typ::Primitive;
use crate::typ::ScopeID;
use crate::typ::SetType;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeName;
use crate::typ::TypeParam;
use crate::typ::TypeParamList;
use crate::typ::SYSTEM_UNIT_NAME;
use crate::IntConstant;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::mem;
use std::sync::Arc;
use terapascal_ir::MetadataSource as _;

impl DigestBuilder<'_> {
    pub fn digest_type_decl(&mut self, id: ir::TypeDefID, decl: &ir::TypeDecl) -> DigestResult<()> {
        match decl {
            ir::TypeDecl::Reserved | ir::TypeDecl::Forward(..) => {
                Ok(())
            }

            ir::TypeDecl::Def(ir::TypeDef::Variant(variant_def)) => {
                self.digest_variant_def(variant_def)?;
                Ok(())
            }

            ir::TypeDecl::Def(ir::TypeDef::Struct(struct_def)) => {
                match &struct_def.identity {
                    ir::StructIdentity::Class(class_path) => {
                        self.digest_struct_def(id, class_path, struct_def, StructKind::Class)
                    }

                    ir::StructIdentity::Record(record_path) => {
                        self.digest_struct_def(id, record_path, struct_def, StructKind::Record)
                    }

                    ir::StructIdentity::Internal(..)
                    | ir::StructIdentity::ClosureObject(..) => {
                        // unnamed class = internal only, no import
                        Ok(())
                    }
                }
            }
        }
    }

    fn digest_variant_def(
        &mut self,
        def: &ir::VariantDef,
    ) -> DigestResult<()> {
        let name = self.digest_def_path(&def.name)?;
        let variant_type = Type::variant(name.clone());

        let def_path = name.full_path.clone();

        let mut cases = Vec::with_capacity(def.cases.len());
        for case_def in &def.cases {
            let data_type = match &case_def.ty {
                None => None,
                Some(t) => {
                    let data_type = self.digest_type(t)?;

                    Some(VariantCaseData {
                        ty: TypeName::Unspecified(data_type),
                    })
                },
            };

            cases.push(VariantCase {
                ident: Ident::new(&case_def.name, self.span()),
                span: self.span(),
                data: data_type,
            });
        }

        // TODO
        let implements = None;
        let where_clause = None;

        let tags = self.digest_tags(&def.tags)?;

        let variant_decl = VariantDecl {
            name: Arc::new(name),
            implements,
            where_clause,

            tags,

            forward: false,

            cases,
            sections: Vec::new(),

            end_kw_span: None,
            span: self.span(),
            kw_span: self.span(),
        };

        self.declare_type(&def.name, variant_type)?;

        self.variant_defs.insert(def_path, variant_decl);

        Ok(())
    }

    fn digest_struct_def(
        &mut self,
        id: ir::TypeDefID,
        name_path: &ir::NamePath,
        def: &ir::StructDef,
        kind: StructKind,
    ) -> DigestResult<()> {
        // if the typedecl is a set, its members are internal and the type info we need for
        // typechecking is stored in the compiler-generated tag
        if self.read_set_type_tag(id).is_some() {
            return Ok(());
        }

        let name = self.digest_def_path(&name_path)?;
        let struct_type = Type::from_struct_type(name.clone(), kind);

        let def_path = name.full_path.clone();

        let mut sections = Vec::with_capacity(def.fields.len());

        for (_, field) in &def.fields {
            let Some(field_name) = &field.name else {
                // skip anonymous internal fields
                continue;
            };

            let field_type = self.digest_type(&field.ty)?;

            sections.push(StructDeclSection {
                members: vec![StructMemberDecl::Field(FieldDecl {
                    span: self.span(),
                    ty: TypeName::Unspecified(field_type),
                    idents: vec![Ident::new(field_name, self.span())],
                    access: Access::Published, // TODO: access modifiers in IR
                })],
                access_kw_span: None,
                access: Access::Published, // TODO: access modifiers in IR
            });
        }

        let tags = self.digest_tags(&def.tags)?;

        // TODO
        let implements = None;
        let where_clause = None;

        let struct_decl = StructDecl {
            name,
            kind,
            implements,
            where_clause,

            tags,

            forward: false,
            packed: def.layout == ir::StructLayout::Packed,

            sections,

            end_kw_span: None,
            span: self.span(),
            kw_span: self.span(),
        };

        self.declare_type(name_path, struct_type)?;

        self.struct_defs.insert(def_path, struct_decl);

        Ok(())
    }

    fn declare_type(&mut self, name_path: &ir::NamePath, as_type: Type) -> DigestResult<()> {
        let span = self.span();

        let unit_scope = if let Some(unit_path) = name_path.parent() {
            self.open_unit(&unit_path)?
        } else {
            ScopeID(0)
        };

        if let Some(ctx) = self.root_ctx.as_mut() {
            let ident = Ident::new(&name_path.path.last().unwrap(), span);
            ctx.declare_type(ident, as_type, Visibility::Interface, true);

            ctx.pop_scope(unit_scope);
        }

        Ok(())
    }

    pub fn digest_type(&mut self, ir_type: &ir::Type) -> DigestResult<Type> {
        let result = match ir_type {
            ir::Type::Nothing => {
                Type::Nothing
            },
            ir::Type::Generic(name) => {
                Type::generic_param(Ident::new(name.as_str(), self.span()))
            },
            ir::Type::Pointer(deref_ty) | ir::Type::TempRef(deref_ty) => {
                self.digest_type(deref_ty)?.ptr()
            }

            ir::Type::Struct(type_ref) | ir::Type::Variant(type_ref) => {
                self.digest_type_ref(type_ref)?
            }

            ir::Type::Array { element, dim } => {
                let element_type = self.digest_type(element)?;
                Type::array(element_type, *dim)
            }

            ir::Type::Object(object_id) => {
                self.digest_object_type(object_id)?
            }
            ir::Type::WeakObject(object_id) => {
                Type::Weak(Arc::new(self.digest_object_type(object_id)?))
            }

            ir::Type::Function(sig) => {
                Type::Function(Arc::new(self.digest_sig(sig)?))
            },

            ir::Type::Bool => Type::Primitive(Primitive::Boolean),
            ir::Type::U8 => Type::Primitive(Primitive::UInt8),
            ir::Type::I8 => Type::Primitive(Primitive::Int8),
            ir::Type::I16 => Type::Primitive(Primitive::Int16),
            ir::Type::U16 => Type::Primitive(Primitive::UInt16),
            ir::Type::I32 => Type::Primitive(Primitive::Int32),
            ir::Type::U32 => Type::Primitive(Primitive::UInt32),
            ir::Type::I64 => Type::Primitive(Primitive::Int64),
            ir::Type::U64 => Type::Primitive(Primitive::UInt64),
            ir::Type::USize => Type::Primitive(Primitive::NativeUInt),
            ir::Type::ISize => Type::Primitive(Primitive::NativeInt),
            ir::Type::F32 => Type::Primitive(Primitive::Real32),
            ir::Type::F64 => Type::Primitive(Primitive::Real64),
        };

        Ok(result)
    }

    #[expect(unused)]
    fn digest_struct_ref(&mut self, type_ref: &ir::TypeRef) -> DigestResult<Symbol> {
        todo!()
    }

    fn digest_object_type(&mut self, object_id: &ir::ObjectID) -> DigestResult<Type> {
        match object_id {
            ir::ObjectID::Any => Ok(Type::Any),

            ir::ObjectID::Class(class_ref) => {
                self.digest_type_ref(class_ref)
            }

            ir::ObjectID::Interface(iface_id) => {
                self.read_interface_type(*iface_id)
            }

            ir::ObjectID::AnyClosure(sig) => {
                let sig = self.digest_sig(sig)?;
                Ok(Type::Function(Arc::new(sig)))
            },

            ir::ObjectID::Array(element_type) => {
                let element_type = self.digest_type(element_type)?;
                Ok(element_type.dyn_array())
            },

            ir::ObjectID::Box(value_type) => {
                let value_type = self.digest_type(value_type)?;
                Ok(value_type.boxed())
            }
        }
    }

    fn digest_type_ref(&mut self, type_ref: &ir::TypeRef) -> DigestResult<Type> {
        if type_ref.args.is_empty()
            && let Some(set_type) = self.read_set_type_tag(type_ref.def_id)
        {
            return Ok(Type::Set(set_type));
        }

        let mut type_args = Vec::with_capacity(type_ref.args.len());
        for type_arg in &type_ref.args {
            type_args.push(TypeName::Unspecified(self.digest_type(type_arg)?));
        }

        let type_decl = self
            .metadata()
            .get_type_decl(type_ref.def_id)
            .ok_or_else(|| {
                let type_name = type_ref.def_id.to_pretty_string(self.metadata());
                DigestError::MissingTypeDef(type_name)
            })?;

        match type_decl {
            ir::TypeDecl::Reserved | ir::TypeDecl::Forward(..) => {
                Err(DigestError::InvalidData("Type is undefined".to_string()))
            }

            ir::TypeDecl::Def(ir::TypeDef::Struct(struct_def)) => {
                match &struct_def.identity {
                    ir::StructIdentity::Record(name)
                    | ir::StructIdentity::Class(name) => {
                        let path = self.digest_def_path(name)?;
                        let kind = if struct_def.identity.is_ref_type() {
                            StructKind::Class
                        } else {
                            StructKind::Record
                        };

                        Ok(Type::from_struct_type(path, kind))
                    }

                    ir::StructIdentity::ClosureObject(..) => {
                        // shouldn't appear in library interface
                        Err(DigestError::UnsupportedFeature("closure types".to_string()))
                    }

                    ir::StructIdentity::Internal(debug_name) => {
                        Err(DigestError::UnsupportedFeature(format!("internal type: {debug_name}")))
                    }
                }
            }

            ir::TypeDecl::Def(ir::TypeDef::Variant(variant_def)) => {
                let path = self.digest_def_path(&variant_def.name)?;
                Ok(Type::variant(path))
            }
        }
    }

    pub fn read_set_type_tag(&mut self, id: ir::TypeDefID) -> Option<Arc<SetType>> {
        if let Some(set_type) = self.set_types.get(&id) {
            return Some(set_type.clone());
        }

        // look up the struct again in each loop since we need mutable access to self
        let tag_count = self.metadata().get_struct_def(id)?.tags.len();

        for i in 0..tag_count {
            let struct_def = self.metadata().get_struct_def(id)?;
            let tag_info = &struct_def.tags[i];

            // check that this tag is the set type tag
            // TODO: cache the ID instead of looking up by name
            let tag_def = self.metadata().get_struct_def(tag_info.class_id)?;

            let tag_struct_name = tag_def.identity.name()?;
            if tag_struct_name.path.len() != 2
                || tag_struct_name.path[0] != SYSTEM_UNIT_NAME
                || tag_struct_name.path[1] != SET_TAG_NAME
            {
                return None;
            }

            let item_type_field_val = tag_info.fields.get(&SET_TAG_ITEM_TYPE_FIELD)?.clone();
            let min_field_val = tag_info.fields.get(&SET_TAG_MIN_FIELD)?.clone();
            let max_field_val = tag_info.fields.get(&SET_TAG_MAX_FIELD)?.clone();

            let name = match struct_def.identity.name() {
                Some(name_path) => {
                    let path = match self.digest_def_path(name_path) {
                        Ok(name) => name.full_path,
                        Err(err) => {
                            let display_path = name_path.to_pretty_string(self.metadata());
                            self.warnings.push(DigestWarning::InvalidPath(display_path, Box::new(err)));
                            return None;
                        }
                    };

                    Some(path)
                }
                None => None,
            };

            let item_type = match item_type_field_val {
                ir::Value::Ref(ir::Ref::Global(ir::GlobalRef::StaticTypeInfo(item_type))) => {
                    self.digest_type(&item_type).ok()?
                }
                _ => return None, // invalid
            };

            let min = match min_field_val {
                ir::Value::LiteralI64(min_lit) => IntConstant::from(min_lit as i128),
                _ => return None,
            };

            let max = match max_field_val {
                ir::Value::LiteralI64(min_lit) => IntConstant::from(min_lit as i128),
                _ => return None,
            };

            let set_type = Arc::new(SetType {
                item_type,
                min,
                max,
                name,
            });

            self.set_types.insert(id, set_type.clone());
            return Some(set_type);
        }

        None
    }

    fn read_interface_type(&mut self, id: ir::InterfaceID) -> DigestResult<Type> {
        let def = self
            .metadata()
            .get_iface_def(id)
            .ok_or_else(|| {
                let type_name = id.to_interface_ptr_type().to_pretty_string(self.metadata());
                DigestError::MissingTypeDef(type_name)
            })?;

        let name = self.digest_def_path(&def.name)?;

        Ok(Type::interface(name))
    }

    fn digest_def_path(&self, name_path: &ir::NamePath) -> DigestResult<Symbol> {
        let ident_path = IdentPath::from_parts(name_path.path
            .iter()
            .map(|part| Ident::new(part, self.span())));

        let type_params = self.digest_def_type_params(&name_path.type_args)?;

        Ok(Symbol::from(ident_path).with_ty_params(type_params))
    }

    pub(super) fn digest_def_type_params(&self, type_params: &[ir::Type]) -> DigestResult<Option<TypeParamList>> {
        if type_params.is_empty() {
            return Ok(None);
        }

        let mut params = Vec::with_capacity(type_params.len());

        for type_arg in type_params {
            let ir::Type::Generic(param_name) = type_arg else {
                let type_name = type_arg.to_pretty_string(self.metadata());
                let msg = format!("definition has invalid type parameter: {type_name}");

                return Err(DigestError::InvalidData(msg));
            };

            params.push(TypeParam {
                span: self.span(),
                name: Ident::new(param_name, self.span()),
                constraint: None,
            });
        }

        Ok(Some(TypeParamList::new(params, self.span())))
    }

    pub fn finish_type_defs(&mut self) {
        let mut type_methods = HashMap::new();
        mem::swap(&mut type_methods, &mut self.type_methods);

        for (declaring_type, type_methods) in type_methods {
            if let Err(err) = self.finish_type_def(&declaring_type, type_methods) {
                self.warnings.push(DigestWarning::InvalidMethodList(declaring_type, Box::new(err)));
            }
        }

        assert!(self.type_methods.is_empty());
    }

    fn finish_type_def(
        &mut self,
        declaring_type: &Type,
        mut method_map: BTreeMap<usize, MethodDecl>,
    ) -> DigestResult<()> {
        let mut method_list = Vec::with_capacity(method_map.len());

        for method_index in 0..method_map.len() {
            let Some(method_decl) = method_map.remove(&method_index) else {
                return Err(DigestError::MissingMethodDef(ir::MethodID(method_index)));
            };

            method_list.push(method_decl)
        }

        match declaring_type {
            Type::Record(path) | Type::Class(path) => {
                self.finish_struct_def(&path.full_path, method_list)
            }

            Type::Variant(path) => {
                self.finish_variant_def(&path.full_path, method_list)
            }

            // Type::Interface(_) => {}

            _ => {
                Err(DigestError::InvalidData(format!("invalid declaring type for method: {declaring_type}",)))
            }
        }
    }

    fn finish_struct_def(
        &mut self,
        path: &IdentPath,
        method_list: Vec<MethodDecl>,
    ) -> DigestResult<()> {
        let struct_def = self.struct_defs
            .get_mut(path)
            .ok_or_else(|| {
                DigestError::MissingTypeDef(path.to_string())
            })?;

        struct_def.sections.push(StructDeclSection {
            members: method_list
                .into_iter()
                .map(|method| StructMemberDecl::Method(method))
                .collect(),
            access: Access::Published, // TODO: access modifiers in IR
            access_kw_span: None,
        });

        Ok(())
    }

    fn finish_variant_def(
        &mut self,
        path: &IdentPath,
        method_list: Vec<MethodDecl>,
    ) -> DigestResult<()> {
        let variant_def = self.variant_defs
            .get_mut(path)
            .ok_or_else(|| {
                DigestError::MissingTypeDef(path.to_string())
            })?;

        variant_def.sections.push(MethodDeclSection {
            methods: method_list,
            access: Access::Published, // TODO: access modifiers in IR
            access_kw_span: None,
        });

        Ok(())
    }
}
