mod read_function;
mod read_type;
mod read_tags;

use self::read_tags::ImportedEnumMember;
use crate::ast::FunctionParamMod;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Literal;
use crate::ast::LiteralItem;
use crate::ast::ObjectCtorArgs;
use crate::ast::Visibility;
use crate::codegen::library_builder::FunctionDeclKey;
use crate::codegen::EnumMemberTagInfo;
use crate::codegen::FunctionInstance;
use crate::codegen::OutParamTagInfo;
use crate::codegen::SetTypeTagInfo;
use crate::import::ImportError;
use crate::import::ImportResult;
use crate::import::ImportWarning;
use crate::ir;
use crate::typ::ast::*;
use crate::typ::*;
use crate::IntConstant;
use crate::RealConstant;
use ir::Metadata;
use ir::MetadataSource as _;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::iter;
use std::sync::Arc;
use terapascal_common::span::Span;

pub(super) struct ImportBuilder<'a> {
    pub library: &'a ir::Library,

    pub imported_funcs: HashMap<FunctionDeclKey, FunctionInstance>,
    pub imported_namespaces: HashSet<IdentPath>,

    pub warnings: Vec<ImportWarning>,

    library_refs: Vec<&'a ir::Library>,

    root_ctx: Option<&'a mut Context>,

    types: HashMap<ir::Type, Type>,

    struct_defs: HashMap<IdentPath, StructDecl>,
    variant_defs: HashMap<IdentPath, VariantDecl>,
    enum_defs: HashMap<IdentPath, EnumDecl>,

    set_types: BTreeMap<ir::TypeDefID, Arc<SetDef>>,

    type_methods: HashMap<Type, BTreeMap<usize, MethodDecl>>,

    default_span: Span,

    enum_member_tag_info: Option<EnumMemberTagInfo>,
    set_type_tag_info: Option<SetTypeTagInfo>,
    out_param_tag_info: Option<OutParamTagInfo>,
}

impl<'a> ImportBuilder<'a> {
    pub fn new(
        library: &'a ir::Library,
        library_refs: impl IntoIterator<Item = &'a ir::Library>,
        type_ctx: Option<&'a mut Context>,
    ) -> Self {
        let mut builder = ImportBuilder {
            library,
            library_refs: library_refs.into_iter().collect(),

            root_ctx: type_ctx,

            imported_namespaces: HashSet::new(),

            types: HashMap::new(),

            struct_defs: HashMap::new(),
            variant_defs: HashMap::new(),
            enum_defs: HashMap::new(),

            set_types: BTreeMap::new(),

            type_methods: HashMap::new(),

            imported_funcs: HashMap::new(),

            warnings: Vec::new(),

            default_span: Span::zero(""),

            enum_member_tag_info: None,
            set_type_tag_info: None,
            out_param_tag_info: None,
        };

        builder.set_type_tag_info = SetTypeTagInfo::find_in_metadata(&builder);
        builder.enum_member_tag_info = EnumMemberTagInfo::find_in_metadata(&builder);
        builder.out_param_tag_info = OutParamTagInfo::find_in_metadata(&builder);

        builder
    }

    pub fn import(&mut self) -> ImportResult<()> {
        self.read_alias_types()?;

        for (type_id, type_decl) in self.library.metadata.type_decls() {
            if let Err(err) = self.read_type_decl(type_id, type_decl) {
                let type_name = type_id.to_pretty_string(self);
                self.warnings.push(ImportWarning::InvalidType(type_name, Box::new(err)));
            }
        }

        for (func_id, func_info) in self.library.metadata.functions() {
            if let Err(err) = self.read_function(func_id, func_info) {
                let func_name = ir::FunctionRef::new(func_id).to_pretty_string(self);
                self.warnings.push(ImportWarning::InvalidFunc(func_name, Box::new(err)));
            }
        }

        for const_info in self.library.metadata.constants() {
            if let Err(err) = self.read_const(const_info) {
                let const_name = const_info.name.to_pretty_string(self);
                self.warnings.push(ImportWarning::InvalidConst(const_name, Box::new(err)));
            }
        }

        self.finish_type_defs();

        Ok(())
    }

    pub fn span(&self) -> Span {
        self.default_span.clone()
    }

    pub fn metadata(&self) -> &impl ir::MetadataCollection {
        self
    }

    pub fn read_tags(&mut self, tags: &[ir::TagInfo]) -> ImportResult<Vec<Tag>> {
        let mut result = Vec::with_capacity(tags.len());
        for tag in tags {
            let tag_type = self.read_type(&tag.class_id.to_class_ptr_type([]))?;

            let tag_def = self
                .get_struct_def(tag.class_id)
                .ok_or_else(|| {
                    let class_name = tag.class_id.to_pretty_string(self);
                    ImportError::MissingTypeDef(class_name)
                })?
                .clone();

            let mut ctor_members =  Vec::with_capacity(tag.fields.len());
            for (field_id, field_val) in &tag.fields {
                let member_name = tag_def.fields
                    .get(field_id)
                    .and_then(|field_def| field_def.name.as_ref())
                    .ok_or_else(|| {
                        let msg = format!("tag object references invalid field index {}", field_id.0);
                        ImportError::InvalidData(msg)
                    })?;

                let member_val = self.read_value(field_val)?;

                ctor_members.push(ObjectCtorMember {
                    ident: Ident::new(member_name, self.span()),
                    value: Expr::Literal(LiteralItem {
                        literal: Literal::Nil, // TODO: placeholder
                        annotation: member_val,
                    }),
                    span: self.span(),
                });
            }

            let item = TagItem {
                span: self.span(),
                tag_type: TypeName::Unspecified(tag_type),
                args: ObjectCtorArgs {
                    span: self.span(),
                    members: ctor_members,
                },
            };

            result.push(Tag {
                span: self.span(),
                items: vec![item]
            })
        }

        Ok(result)
    }

    pub fn read_const(
        &mut self,
        const_info: &ir::ConstInfo,
    ) -> ImportResult<()> {
        let path = self.read_ident_path(&const_info.name);

        if !const_info.name.type_args.is_empty() {
            let name_display = const_info.name.to_pretty_string(self);
            let msg = format!("path of constant item {path} has type parameters: {name_display}");
            return Err(ImportError::InvalidData(msg))
        }

        let name_ident = path.last().clone();
        let span = self.span();

        let const_val = match self.read_value(&const_info.value)? {
            Value::Const(val) => val,
            other => {
                let msg = format!("value of constant item {path} could not be converted to a constant: {other}");
                return Err(ImportError::InvalidData(msg));
            }
        };

        for tag in &const_info.tags {
            // if this const is tagged as an enum member, declare it as part of an enum decl instead
            if let Some(member_tag) = self.read_enum_member_tag(tag) {
                self.read_const_as_enum_member(path, const_val, member_tag)?;
                return Ok(());
            }
        }

        let unit_path = path
            .parent()
            .ok_or_else(|| ImportError::InvalidData(format!("path {path} does not contain a unit namespace")))?;

        let value_type = self.read_type(&const_info.value_type)?;

        self.with_unit_scope(unit_path, |builder| {
            let visibility = Visibility::Interface; // TODO: access modifiers in IR

            if let Some(ctx) = builder.root_ctx.as_mut() {
                ctx.declare_global_const(name_ident, const_val.value.clone(), value_type, visibility, span)?;
            }
            Ok(())
        })?;

        Ok(())
    }

    fn read_const_as_enum_member(&mut self,
        path: IdentPath,
        const_val: Arc<ConstValue>,
        enum_member: ImportedEnumMember,
    ) -> ImportResult<()> {
        let name_ident = path.last().clone();
        let span = self.span();

        let enum_path = match self.get_type_decl(enum_member.enum_type_id) {
            Some(ir::TypeDecl::Forward(enum_path)) if enum_path.type_args.is_empty() => {
                enum_path.clone()
            },
            _ => {
                return Err(ImportError::MissingTypeDef(format!("enum type of {path}")));
            },
        };

        let enum_name = self.read_ident_path(&enum_path);

        let enum_def = self.enum_defs
            .entry(enum_name)
            .or_insert_with_key(|enum_name| EnumDecl {
                span,
                name: Arc::new(Symbol::from(enum_name.clone())),
                items: Vec::new(),
            });

        let Some(ord_val) = const_val.value.clone().try_into_int() else {
            let msg = format!("value of constant item {path} is not an ordinal value: {}", const_val.value);
            return Err(ImportError::InvalidData(msg));
        };

        enum_def.items.push(EnumDeclItem {
            annotation: Value::Const(const_val.clone()),
            value: Some(EvaluatedConstExpr {
                value: ord_val,
                expr: Box::new(Expr::Ident(name_ident.clone(), Value::Const(const_val))),
            }),
            ident: name_ident,
        });

        Ok(())
    }

    pub fn read_sig(&mut self, sig: &ir::FunctionSig) -> ImportResult<FunctionSig> {
        let result_ty = self.read_type(&sig.result_type)?;

        let mut params = Vec::with_capacity(sig.param_types.len());
        for param_type in &sig.param_types {
            let (param_type, param_mod) = match param_type {
                ir::Type::TempRef(deref_type) => {
                    (self.read_type(deref_type)?, Some(FunctionParamMod::Var))
                }

                t => {
                    (self.read_type(t)?, None)
                }
            };

            params.push(FunctionSigParam {
                ty: param_type,
                modifier: param_mod,
            });
        }

        Ok(FunctionSig {
            result_ty,
            type_params: None,
            params,
        })
    }

    fn read_value(&mut self, ir_value: &ir::Value) -> ImportResult<Value> {
        let result = match ir_value {
            ir::Value::LiteralNil => {
                Value::from(ConstValue::literal(Literal::Nil, Type::Nothing.ptr(), self.span()))
            }
            ir::Value::LiteralBool(val) => {
                Value::from(ConstValue::literal(Literal::Boolean(*val), Type::Nothing.ptr(), self.span()))
            }

            ir::Value::LiteralU8(val) => self.make_int_literal(*val, Primitive::UInt8),
            ir::Value::LiteralI8(val) => self.make_int_literal(*val, Primitive::Int8),
            ir::Value::LiteralI16(val) => self.make_int_literal(*val, Primitive::Int16),
            ir::Value::LiteralU16(val) => self.make_int_literal(*val, Primitive::UInt16),
            ir::Value::LiteralI32(val) => self.make_int_literal(*val, Primitive::Int32),
            ir::Value::LiteralU32(val) => self.make_int_literal(*val, Primitive::UInt32),
            ir::Value::LiteralI64(val) => self.make_int_literal(*val, Primitive::Int64),
            ir::Value::LiteralU64(val) => self.make_int_literal(*val, Primitive::UInt64),
            ir::Value::LiteralISize(val) => self.make_int_literal(*val, Primitive::NativeInt),
            ir::Value::LiteralUSize(val) => self.make_int_literal(*val, Primitive::NativeUInt),
            ir::Value::LiteralF32(val) => self.make_real_literal(*val, Primitive::Real32),
            ir::Value::LiteralF64(val) => self.make_real_literal(*val, Primitive::Real64),

            ir::Value::SizeOf(of_type) => {
                let of_type = TypeName::Unspecified(self.read_type(of_type)?);
                let size_lit = Literal::SizeOf(Box::new(of_type));

                Value::from(ConstValue::literal(size_lit, Primitive::Int32, self.span()))
            }
            ir::Value::Default(of_type) => {
                let of_type = TypeName::Unspecified(self.read_type(of_type)?);
                let default_lit = Literal::DefaultValue(Box::new(of_type));

                Value::from(ConstValue::literal(default_lit, Primitive::Int32, self.span()))
            }
            ir::Value::Ref(r) => {
                self.read_ref(r)?
            }
        };

        Ok(result)
    }

    fn make_int_literal(&self, val: impl Into<IntConstant>, ty: impl Into<Type>) -> Value {
        let literal = Literal::Integer(val.into());

        Value::from(ConstValue::literal(literal, ty, self.span()))
    }

    fn make_real_literal(&self, val: impl Into<RealConstant>, ty: impl Into<Type>) -> Value {
        let literal = Literal::Real(val.into());

        Value::from(ConstValue::literal(literal, ty, self.span()))
    }

    fn read_ref(&mut self, r: &ir::Ref) -> ImportResult<Value> {
        match r {
            ir::Ref::Global(ir::GlobalRef::Variable(id)) => {
                let var_info = self.library
                    .metadata()
                    .get_variable(*id)
                    .ok_or_else(|| {
                        let msg = format!("reference to missing global variable {}", id.0);
                        ImportError::InvalidData(msg)
                    })?;

                let var_type = self.read_type(&var_info.value_type)?;

                // there should be no references to unnamed vars in a package's interface
                let Some(name_path) = &var_info.name else {
                    let msg = format!("reference to anonymous global variable {}", id.0);
                    return Err(ImportError::InvalidData(msg));
                };

                let decl = IdentPath::from_parts(name_path.path
                    .iter()
                    .map(|part| Ident::new(part, self.span())));

                let var_value = TypedValue::unit_var(var_type, decl, self.span());
                Ok(Value::from(var_value))
            }

            ir::Ref::Global(ir::GlobalRef::StringLiteral(string_id)) => {
                let string_text = self
                    .metadata()
                    .get_string(*string_id)
                    .ok_or_else(|| ImportError::MissingString(*string_id))?;

                let string_val = ConstValue::string_literal(string_text.clone(), self.span());
                Ok(Value::from(string_val))
            }

            ir::Ref::Global(ir::GlobalRef::StaticTypeInfo(ty)) => {
                let typeinfo_type = self.read_type(&ty)?;
                let typeinfo_literal = Literal::TypeInfo(Box::new(TypeName::Unspecified(typeinfo_type)));
                let typeinfo_class = Type::class(builtin_typeinfo_name());

                Ok(Value::from(ConstValue::literal(typeinfo_literal, typeinfo_class, self.span())))
            }

            // most ref types should never appear in the interface and can be ignored
            _ => {
                Err(ImportError::UnsupportedRef(r.to_pretty_string(self.metadata())))
            },
        }
    }

    pub fn read_ident_path(&self, path: &ir::NamePath) -> IdentPath {
        IdentPath::from_parts(path.path.iter()
            .map(|part| Ident::new(part, self.span())))
    }

    pub fn open_unit(&mut self, unit_path: IdentPath) -> ImportResult<ScopeID> {
        self.imported_namespaces.insert(unit_path.clone());

        if let Some(ctx) = self.root_ctx.as_mut() {
            let scope_id = ctx.push_unit_scope(unit_path)?;
            Ok(scope_id)
        } else {
            Ok(ScopeID(0))
        }
    }

    pub fn with_unit_scope<F, T>(&mut self, unit_path: IdentPath, f: F) -> ImportResult<T>
    where
        F: FnOnce(&mut Self) -> ImportResult<T>
    {
        let scope_id = self.open_unit(unit_path)?;

        let result = f(self);

        if !scope_id.is_root() && let Some(root_ctx) = self.root_ctx.as_mut() {
            root_ctx.pop_scope(scope_id);
        }

        result
    }
}

impl<'a> ir::MetadataCollection for ImportBuilder<'a> {
    fn all_metadata(&self) -> impl Iterator<Item=&Metadata> {
        self.library_refs
            .iter()
            .rev()
            .map(|ref_lib| ref_lib.metadata.as_ref())
            .chain(iter::once(self.library.metadata.as_ref()))
    }
}