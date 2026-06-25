use crate::ast::Access;
use crate::ast::FunctionDeclKind;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::StructKind;
use crate::ast::Visibility;
use crate::codegen::ALIAS_TAG_CLASS_NAME;
use crate::codegen::ALIAS_TAG_NAME_FIELD;
use crate::codegen::ALIAS_TAG_TARGET_FIELD;
use crate::import::ImportBuilder;
use crate::import::ImportError;
use crate::import::ImportResult;
use crate::import::ImportWarning;
use crate::ir;
use crate::typ::ast::*;
use crate::typ::Primitive;
use crate::typ::ScopeID;
use crate::typ::SetDef;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeArgList;
use crate::typ::TypeName;
use crate::typ::TypeParam;
use crate::typ::TypeParamList;
use crate::typ::SYSTEM_UNIT_NAME;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_ir::MetadataSource as _;
use terapascal_ir::StructIdentity;

impl ImportBuilder<'_> {
    pub fn read_type_decl(&mut self, id: ir::TypeDefID, decl: &ir::TypeDecl) -> ImportResult<()> {
        match decl {
            ir::TypeDecl::Reserved | ir::TypeDecl::Forward(..) => {
                Ok(())
            }

            ir::TypeDecl::Def(ir::TypeDef::Variant(variant_def)) => {
                self.read_variant_def(id, variant_def)?;
                Ok(())
            }

            ir::TypeDecl::Def(ir::TypeDef::Struct(struct_def)) => {
                match &struct_def.identity {
                    ir::StructIdentity::Class(class_path) => {
                        self.read_struct_def(id, class_path, struct_def, StructKind::Class)
                    }

                    ir::StructIdentity::Record(record_path) => {
                        self.read_struct_def(id, record_path, struct_def, StructKind::Record)
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

    fn read_variant_def(
        &mut self,
        id: ir::TypeDefID,
        def: &ir::VariantDef,
    ) -> ImportResult<()> {
        let name = self.read_def_path(&def.name)?;
        let variant_type = Type::variant(name.clone());

        let def_path = name.full_path.clone();

        let mut cases = Vec::with_capacity(def.cases.len());
        for case_def in &def.cases {
            let data_type = match &case_def.ty {
                None => None,
                Some(t) => {
                    let data_type = self.read_type(t)?;

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

        let def_type = id.to_variant_type(def.name.type_args.clone());
        let implements = self.read_type_impls(&def_type)?;

        let where_clause = None; // TODO: generic constraints in IR

        let tags = self.read_tags(&def.tags)?;

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

    fn read_type_impls(&mut self, ty: &ir::Type) -> ImportResult<Option<SupersClause>> {
        let iface_impls: Vec<_> = self
            .type_impls(ty)
            .into_iter()
            .map(|(iface_id, _)| iface_id)
            .collect();

        if iface_impls.is_empty() {
            return Ok(None);
        }

        let mut types = Vec::with_capacity(iface_impls.len());
        for iface_id in iface_impls {
            let iface_type = self.read_type(&iface_id.to_interface_ptr_type())?;

            types.push(TypeName::Unspecified(iface_type));
        }

        Ok(Some(SupersClause {
            span: self.span(),
            kw_span: self.span(),
            types,
        }))
    }

    fn read_struct_def(
        &mut self,
        id: ir::TypeDefID,
        name_path: &ir::NamePath,
        def: &ir::StructDef,
        kind: StructKind,
    ) -> ImportResult<()> {
        // if the typedecl is a set, its members are internal and the type info we need for
        // typechecking is stored in the compiler-generated tag
        if self.read_type_as_set(id).is_some() {
            return Ok(());
        }

        let name = self.read_def_path(&name_path)?;
        let struct_type = Type::from_struct_type(name.clone(), kind);

        let def_path = name.full_path.clone();

        let mut sections = Vec::with_capacity(def.fields.len());

        for (_, field) in &def.fields {
            let Some(field_name) = &field.name else {
                // skip anonymous internal fields
                continue;
            };

            let field_type = self.read_type(&field.ty)?;

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

        let tags = self.read_tags(&def.tags)?;

        let implements = self.read_type_impls(&def.identity.to_definition_type(id))?;

        let where_clause = None; // TODO: generic constraints in IR

        let struct_decl = StructDecl {
            name: Arc::new(name),
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

    fn declare_type(&mut self, name_path: &ir::NamePath, as_type: Type) -> ImportResult<()> {
        let span = self.span();

        let unit_scope = if let Some(unit_path) = name_path.parent() {
            self.open_unit(self.read_ident_path(&unit_path))?
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

    pub fn read_type(&mut self, ir_type: &ir::Type) -> ImportResult<Type> {
        if let Some(cached) = self.types.get(ir_type) {
            return Ok(cached.clone());
        }

        let result = match ir_type {
            // don't need to cache these
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

            ir::Type::Nothing => Type::Nothing,

            ir::Type::TempRef(..) => {
                // these should be handled before calling read_type, so if we reach here, a ref
                // has appeared somewhere it can't be handled by Pascal
                let type_name = ir_type.to_pretty_string(self.metadata());
                let msg = format!("temporary reference type ({type_name}) cannot be imported");

                return Err(ImportError::InvalidData(msg));
            }

            ir::Type::Generic(name) => {
                let param_type = Type::generic_param(Ident::new(name.as_str(), self.span()));
                self.types.insert(ir_type.clone(), param_type.clone());

                param_type
            },

            ir::Type::Pointer(deref_ty) => {
                let deref_type = self.read_type(deref_ty)?;

                let pointer_type = if deref_type == Type::Nothing {
                    Type::Primitive(Primitive::Pointer)
                } else {
                    deref_type.ptr()
                };

                self.types.insert(ir_type.clone(), pointer_type.clone());

                pointer_type
            }

            ir::Type::Struct(type_ref) | ir::Type::Variant(type_ref) => {
                self.read_type_ref(type_ref)?
            }

            ir::Type::Array { element, dim } => {
                let element_type = self.read_type(element)?;
                Type::array(element_type, *dim)
            }

            ir::Type::Object(object_id) => {
                self.build_object_type(object_id)?
            }

            ir::Type::WeakObject(object_id) => {
                let weak_type = Type::Weak(Arc::new(self.build_object_type(object_id)?));
                self.types.insert(ir_type.clone(), weak_type.clone());
                weak_type
            }

            ir::Type::Function(sig) => {
                let func_type = Type::Function(Arc::new(self.read_sig(sig)?));
                self.types.insert(ir_type.clone(), func_type.clone());
                func_type
            },
        };

        Ok(result)
    }

    fn build_object_type(&mut self, object_id: &ir::ObjectID) -> ImportResult<Type> {
        let strong_type = object_id.to_object_type();
        let weak_type = object_id.to_weak_object_type();

        let result_type = match object_id {
            ir::ObjectID::Any => {
                Type::Any
            },

            ir::ObjectID::AnyClosure(sig) => {
                let sig = self.read_sig(sig)?;

                let func_type = Type::Function(Arc::new(sig));

                self.types.insert(strong_type.clone(), func_type.clone());
                self.types.insert(weak_type.clone(), func_type.to_weak());

                func_type
            },

            ir::ObjectID::Array(element_type) => {
                let element_type = self.read_type(element_type)?;

                let array_type = element_type.dyn_array();
                self.types.insert(object_id.to_object_type(), array_type.clone());
                self.types.insert(object_id.to_weak_object_type(), array_type.to_weak());

                array_type
            },

            ir::ObjectID::Box(value_type) => {
                let value_type = self.read_type(value_type)?;

                let box_type = value_type.boxed();
                self.types.insert(object_id.to_object_type(), box_type.clone());
                self.types.insert(object_id.to_weak_object_type(), box_type.to_weak());

                box_type
            }

            ir::ObjectID::Class(class_ref) => {
                let class_type = self.read_type_ref(class_ref)?;

                assert!(class_type.is_object());
                assert!(self.types.contains_key(&object_id.to_object_type()));
                assert!(self.types.contains_key(&object_id.to_object_type()));

                class_type
            }

            ir::ObjectID::Interface(iface_id) => {
                let iface_type = self.read_iface_type(*iface_id)?;

                assert!(iface_type.as_iface().is_some());
                assert!(self.types.contains_key(&object_id.to_object_type()));
                assert!(self.types.contains_key(&object_id.to_object_type()));

                iface_type
            }
        };

        assert!(
            result_type.is_object(),
            "expected type {} to be translated as an object type, was {}",
            object_id.to_object_type().to_pretty_string(self),
            result_type
        );

        Ok(result_type)
    }

    fn read_type_ref(&mut self, type_ref: &Rc<ir::TypeRef>) -> ImportResult<Type> {
        if let Some(set_type) = self.read_type_as_set(type_ref.def_id) {
            let ty = Type::Set(set_type);
            self.types.insert(type_ref.to_struct_type(), ty.clone());

            if !type_ref.args.is_empty() {
                let err = ImportError::InvalidData("type def with set type tag must not have type arguments".to_string());
                self.warnings.push(ImportWarning::InvalidType(ty.to_string(), Box::new(err)));
            }

            return Ok(ty);
        }

        let type_args_list = if type_ref.args.is_empty() {
            None
        } else {
            let mut type_args = Vec::with_capacity(type_ref.args.len());
            for type_arg in &type_ref.args {
                type_args.push(TypeName::Unspecified(self.read_type(type_arg)?));
            }
            Some(TypeArgList::new(type_args, self.span()))
        };

        let type_decl = self
            .metadata()
            .get_type_decl(type_ref.def_id)
            .ok_or_else(|| {
                let type_name = type_ref.def_id.to_pretty_string(self.metadata());
                ImportError::MissingTypeDef(type_name)
            })?;

        match type_decl {
            ir::TypeDecl::Reserved | ir::TypeDecl::Forward(..) => {
                Err(ImportError::InvalidData("Type is undefined".to_string()))
            }

            ir::TypeDecl::Def(ir::TypeDef::Struct(struct_def)) => {
                match &struct_def.identity {
                    ir::StructIdentity::Record(name)
                    | ir::StructIdentity::Class(name) => {
                        let path = self.read_def_path(name)?;

                        match struct_def.identity {
                            StructIdentity::Internal(_) | StructIdentity::Record(_) => {
                                let struct_type = Type::from_struct_type(path.with_ty_args(type_args_list), StructKind::Record);
                                self.types.insert(type_ref.to_struct_type(), struct_type.clone());
                                Ok(struct_type)
                            }

                            StructIdentity::Class(_) | StructIdentity::ClosureObject(_) => {
                                let class_type = Type::from_struct_type(path.with_ty_args(type_args_list), StructKind::Class);
                                self.types.insert(type_ref.to_class_object_type(), class_type.clone());
                                self.types.insert(type_ref.to_weak_class_object_type(), class_type.to_weak());
                                Ok(class_type)
                            }
                        }
                    }

                    ir::StructIdentity::ClosureObject(..) => {
                        // shouldn't appear in library interface
                        Err(ImportError::UnsupportedFeature("closure types".to_string()))
                    }

                    ir::StructIdentity::Internal(debug_name) => {
                        Err(ImportError::UnsupportedFeature(format!("internal type: {debug_name}")))
                    }
                }
            }

            ir::TypeDecl::Def(ir::TypeDef::Variant(variant_def)) => {
                let path = self.read_def_path(&variant_def.name)?;
                let variant_type = Type::variant(path.with_ty_args(type_args_list));

                self.types.insert(type_ref.to_variant_type(), variant_type.clone());

                Ok(variant_type)
            }
        }
    }

    pub fn read_type_as_set(&mut self, id: ir::TypeDefID) -> Option<Arc<SetDef>> {
        if let Some(set_type) = self.set_types.get(&id) {
            return Some(set_type.clone());
        }

        let struct_def = self.metadata().get_struct_def(id)?.clone();

        let tag_count = struct_def.tags.len();

        for i in 0..tag_count {
            let tag_info = &struct_def.tags[i];

            let imported_set_info = self.read_set_tag(&tag_info)?;

            let name = match struct_def.identity.name() {
                Some(name_path) => {
                    let path = match self.read_def_path(name_path) {
                        Ok(name) => name.full_path,
                        Err(err) => {
                            let display_path = name_path.to_pretty_string(self.metadata());
                            self.warnings.push(ImportWarning::InvalidPath(display_path, Box::new(err)));
                            return None;
                        }
                    };

                    Some(path)
                }
                None => None,
            };

            let set_type = Arc::new(SetDef {
                item_type: imported_set_info.item_type,
                min: imported_set_info.min,
                max: imported_set_info.max,
                name: name.clone(),
            });

            self.set_types.insert(id, set_type.clone());

            return Some(set_type);
        }

        None
    }

    fn read_iface_type(&mut self, id: ir::InterfaceID) -> ImportResult<Type> {
        let def = self
            .metadata()
            .get_iface_def(id)
            .cloned()
            .ok_or_else(|| {
                let type_name = id.to_interface_ptr_type().to_pretty_string(self.metadata());
                ImportError::MissingTypeDef(type_name)
            })?;

        let tags = self.read_tags(&def.tags)?;
        let name = self.read_def_path(&def.name)?;

        let iface_type = Type::interface(name.clone());

        self.types.insert(id.to_interface_ptr_type(), iface_type.clone());

        let mut methods = Vec::with_capacity(def.methods.len());

        for method in &def.methods {
            let method_ident = Ident::new(&method.name, self.span());
            let method_name = FunctionName::new_method_decl(
                method_ident,
                None,
                iface_type.clone(),
                None,
            );

            // TODO: interface method decls can't have tags yet, only impls can
            let method_tags = Vec::new();

            let mut result_type = self.read_type(&method.return_ty)?;
            let mut param_groups = self.read_params(&method.params)?;

            // TODO: rework self refs
            // is this ambiguous if a method uses both a Self ref and the name of the interface?
            // in interface method signatures, a reference to the interface type itself is
            // read as a Self type
            if result_type == iface_type {
                result_type = Type::MethodSelf;
            }
            for param in &mut param_groups {
                if param.ty == iface_type {
                    param.ty = TypeName::Unspecified(Type::MethodSelf);
                }
            }

            methods.push(InterfaceMethodDecl {
                decl: Arc::new(FunctionDecl {
                    name: method_name,
                    tags: method_tags,
                    kind: FunctionDeclKind::Function,
                    span: self.span(),
                    param_groups,
                    result_ty: TypeName::Unspecified(result_type),
                    mods: vec![],
                    kw_span: None,
                    where_clause: None,
                }),
            });
        }

        let name = Arc::new(name);

        let iface_decl = InterfaceDecl {
            name: name.clone(),
            tags,
            methods,
            supers: None,
            forward: false,
            where_clause: None, // TODO: generic constraints in IR
            end_kw_span: None,
            kw_span: self.span(),
            span: self.span(),
        };

        self.declare_type_def_with(&name.full_path, |builder| {
            let Some(root_ctx) = builder.root_ctx.as_mut() else {
                return Ok(());
            };

            let visibility = Visibility::Interface; // TODO: access modifiers in IR

            if let Err(err) = root_ctx.declare_iface(Arc::new(iface_decl), visibility) {
                let msg = format!("Declaring interface type {} failed", iface_type);
                builder.warnings.push(ImportWarning::InvalidType(msg, Box::new(ImportError::from(err))));
            }

            Ok(())
        })?;

        Ok(iface_type)
    }

    pub fn read_def_path(&self, name_path: &ir::NamePath) -> ImportResult<Symbol> {
        let ident_path = self.read_ident_path(&name_path);

        let type_params = self.read_def_type_params(&name_path.type_args)?;

        Ok(Symbol::from(ident_path).with_ty_params(type_params))
    }

    pub(super) fn read_def_type_params(&self, type_params: &[ir::Type]) -> ImportResult<Option<TypeParamList>> {
        if type_params.is_empty() {
            return Ok(None);
        }

        let mut params = Vec::with_capacity(type_params.len());

        for type_arg in type_params {
            let ir::Type::Generic(param_name) = type_arg else {
                let type_name = type_arg.to_pretty_string(self.metadata());
                let msg = format!("definition has invalid type parameter: {type_name}");

                return Err(ImportError::InvalidData(msg));
            };

            params.push(TypeParam {
                span: self.span(),
                name: Ident::new(param_name, self.span()),
                constraint: None,
            });
        }

        Ok(Some(TypeParamList::new(params, self.span())))
    }

    pub fn read_alias_types(&mut self) -> ImportResult<()> {
        if self.root_ctx.is_none() {
            // aliases only get used during typechecking
            return Ok(());
        }

        // the alias tag class must be defined for this
        let alias_tag_name = ir::NamePath::new([SYSTEM_UNIT_NAME.to_string()], ALIAS_TAG_CLASS_NAME);

        let (alias_tag_class_id, alias_tag_def) = self
            .metadata()
            .find_struct_def(&alias_tag_name)
            .ok_or_else(|| ImportError::UnsupportedFeature("alias tag class definition is not available".to_string()))?;

        let name_field = alias_tag_def
            .find_field(ALIAS_TAG_NAME_FIELD)
            .ok_or_else(|| {
                let msg = format!("alias tag class definition is missing field: `{ALIAS_TAG_NAME_FIELD}`");
                ImportError::UnsupportedFeature(msg)
            })?;

        let target_field = alias_tag_def
            .find_field(ALIAS_TAG_TARGET_FIELD)
            .ok_or_else(|| {
                let msg = format!("alias tag class definition is missing field: `{ALIAS_TAG_TARGET_FIELD}`");
                ImportError::UnsupportedFeature(msg)
            })?;

        for lib_tag in &self.library.tags {
            if let Err(err) = self.read_alias_type_tag(lib_tag, alias_tag_class_id, name_field, target_field) {
                self.warnings.push(ImportWarning::InvalidLibTag(Box::new(err)));
            }
        }

        Ok(())
    }

    fn read_alias_type_tag(
        &mut self,
        tag: &ir::TagInfo,
        alias_tag_class_id: ir::TypeDefID,
        name_field: ir::FieldID,
        target_field: ir::FieldID,
    ) -> ImportResult<()> {
        if tag.class_id != alias_tag_class_id {
            return Ok(());
        }

        let Some(name_val) = tag.fields.get(&name_field) else {
            let msg = "missing alias tag name field".to_string() ;
            return Err(ImportError::InvalidData(msg));
        };

        let Some(target_val) = tag.fields.get(&target_field) else {
            let msg = "missing alias tag target field".to_string() ;
            return Err(ImportError::InvalidData(msg));
        };

        let ir::Value::Ref(ir::Ref::Global(ir::GlobalRef::StringLiteral(name_string))) = name_val else {
            let msg = "invalid value for alias tag name field".to_string() ;
            return Err(ImportError::InvalidData(msg))
        };

        let Some(name) = self.metadata().get_string(*name_string).cloned() else {
            return Err(ImportError::MissingString(*name_string))
        };

        let ir::Value::Ref(ir::Ref::Global(ir::GlobalRef::StaticTypeInfo(target_type))) = target_val else {
            let msg = "invalid value for alias tag target field".to_string();
            return Err(ImportError::InvalidData(msg))
        };
        let target_type = self.read_type(target_type)?;

        let mut name_path: Vec<_> = name.split('.')
            .map(|part| Ident::new(part, self.span()))
            .collect();

        let Some(alias_name) = name_path.pop() else {
            let msg = "empty alias name".to_string();
            return Err(ImportError::InvalidData(msg));
        };

        if name_path.is_empty() {
            let msg = format!("alias name missing namespace: {name}");
            return Err(ImportError::InvalidData(msg));
        };
        let unit_path = IdentPath::from_parts(name_path);

        let aliased_path = target_type
            .full_name()
            .map(|name| name.full_path.clone())
            .ok_or_else(|| {
                ImportError::InvalidData(format!("invalid alias target type: {}", target_type))
            })?;

        let scope_id  = self.open_unit(unit_path)?;

        let Some(root_ctx) = self.root_ctx.as_mut() else {
            return Ok(());
        };

        root_ctx.declare_alias(alias_name, aliased_path)?;

        root_ctx.pop_scope(scope_id);

        Ok(())
    }

    pub fn finish_type_defs(&mut self) {
        // add all imported method functions to their appropriate declaring types as members
        let mut type_methods = HashMap::new();
        mem::swap(&mut type_methods, &mut self.type_methods);

        for (declaring_type, type_method_map) in &type_methods {
            // primitive types don't have definitions, so their methods are inherent and don't
            // need to be declared anywhere
            if declaring_type.as_primitive().is_some() {
                continue;
            }

            if let Err(err) = self.finish_type_def(declaring_type, type_method_map) {
                self.warnings.push(ImportWarning::InvalidMethodList(
                    declaring_type.clone(),
                    type_method_map.clone(),
                    Box::new(err),
                ));
            }
        }

        assert!(self.type_methods.is_empty());

        let struct_defs: Vec<_> = self.struct_defs.drain().collect();
        for (path, struct_def) in struct_defs {
            let struct_def = Arc::new(struct_def);

            if let Err(err) = self.declare_struct_def(&path, struct_def.clone()) {
                let msg = format!("Declaring struct type {} failed", struct_def.name);

                self.warnings.push(ImportWarning::InvalidType(msg, Box::new(ImportError::from(err))));
            }
        }

        let variant_defs: Vec<_> = self.variant_defs.drain().collect();
        for (path, variant_def) in variant_defs {
            let variant_def = Arc::new(variant_def);

            if let Err(err) = self.declare_variant_def(&path, variant_def.clone()) {
                let msg = format!("Declaring variant type {} failed", variant_def.name);
                self.warnings.push(ImportWarning::InvalidType(msg, Box::new(ImportError::from(err))));
            }
        }

        let enum_defs: Vec<_> = self.enum_defs.drain().collect();
        for (path, enum_def) in enum_defs {
            let enum_def = Arc::new(enum_def);
            let visibility =  Visibility::Interface; // TODO: access modifiers in IR

            let result = self.declare_type_def_with(&path, |builder| {
                let Some(ctx) = builder.root_ctx.as_mut() else {
                    return Ok(());
                };

                ctx.declare_enum(enum_def.clone(), visibility)?;
                Ok(())
            });

            if let Err(err) = result {
                let msg = format!("Declaring enum type {} failed", enum_def.name);
                self.warnings.push(ImportWarning::InvalidType(msg, Box::new(ImportError::from(err))));
            }
        }

        for (declaring_type, type_method_map) in &type_methods {
            for (_, method_decl) in type_method_map {
                let Some(ctx) = self.root_ctx.as_mut() else {
                    continue;
                };

                let func_decl = method_decl.func_decl.clone();

                if let Err(err) = ctx.define_external_method(declaring_type.clone(), func_decl) {
                    let import_err = Box::new(ImportError::from(err));

                    self.warnings.push(ImportWarning::InvalidMethodList(
                        declaring_type.clone(),
                        type_method_map.clone(),
                        import_err,
                    ));
                }
            }
        }
    }

    fn declare_type_def_with<F>(&mut self, type_path: &IdentPath, f: F) -> ImportResult<()>
        where F: FnOnce(&mut Self) -> ImportResult<()>
    {
        let Some(unit_path) = type_path.parent() else {
            return Err(ImportError::InvalidData(format!("invalid type decl path: {type_path}")));
        };

        self.with_unit_scope(unit_path, |builder| {
            f(builder)
        })?;

        Ok(())
    }

    fn declare_struct_def(&mut self, path: &IdentPath, struct_def: Arc<StructDecl>) -> ImportResult<()> {
        self.declare_type_def_with(path, |builder| {
            let visibility =  Visibility::Interface; // TODO: access modifiers in IR

            let Some(root_ctx) = builder.root_ctx.as_mut() else {
                return Ok(());
            };

            root_ctx.declare_struct(struct_def, visibility)?;
            Ok(())
        })
    }

    fn declare_variant_def(&mut self, path: &IdentPath, variant_def: Arc<VariantDecl>) -> ImportResult<()> {
        self.declare_type_def_with(path, |builder| {
            let visibility =  Visibility::Interface; // TODO: access modifiers in IR

            let Some(root_ctx) = builder.root_ctx.as_mut() else {
                return Ok(());
            };

            root_ctx.declare_variant(variant_def, visibility)?;
            Ok(())
        })
    }

    fn finish_type_def(
        &mut self,
        declaring_type: &Type,
        method_map: &BTreeMap<usize, MethodDecl>,
    ) -> ImportResult<()> {
        let mut method_list = Vec::with_capacity(method_map.len());

        for method_index in 0..method_map.len() {
            let Some(method_decl) = method_map.get(&method_index) else {
                return Err(ImportError::MissingMethodDef(ir::MethodID(method_index)));
            };

            method_list.push(method_decl.clone())
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
                Err(ImportError::InvalidData(format!("invalid declaring type for method: {declaring_type}",)))
            }
        }
    }

    fn finish_struct_def(
        &mut self,
        path: &IdentPath,
        method_list: Vec<MethodDecl>,
    ) -> ImportResult<()> {
        let struct_def = self.struct_defs
            .get_mut(path)
            .ok_or_else(|| {
                ImportError::MissingTypeDef(path.to_string())
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
    ) -> ImportResult<()> {
        let variant_def = self.variant_defs
            .get_mut(path)
            .ok_or_else(|| {
                ImportError::MissingTypeDef(path.to_string())
            })?;

        variant_def.sections.push(MethodDeclSection {
            methods: method_list,
            access: Access::Published, // TODO: access modifiers in IR
            access_kw_span: None,
        });

        Ok(())
    }
}