mod digest_function;
mod digest_type;
mod error;

pub use self::error::*;
use crate::ast::FunctionParamMod;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::Literal;
use crate::ast::LiteralItem;
use crate::ast::ObjectCtorArgs;
use crate::ir;
use crate::typ::ast::Expr;
use crate::typ::ast::ObjectCtorMember;
use crate::typ::ast::Tag;
use crate::typ::ast::TagItem;
use crate::typ::builtin_typeinfo_name;
use crate::typ::ConstValue;
use crate::typ::Context;
use crate::typ::FunctionSig;
use crate::typ::FunctionSigParam;
use crate::typ::Primitive;
use crate::typ::ScopeID;
use crate::typ::Type;
use crate::typ::TypeName;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::IntConstant;
use crate::RealConstant;
use std::collections::HashSet;
use std::fs;
use std::io::Read;
use std::path::Path;
use terapascal_common::span::Span;
use terapascal_ir::decode_lib;
use terapascal_ir::MetadataSource as _;

#[derive(Debug)]
pub struct DigestOutput {
    pub library: ir::Library,

    pub warnings: Vec<DigestWarning>,

    pub namespaces: HashSet<IdentPath>,
}

struct DigestBuilder<'a> {
    digest_span: Span,

    library: &'a ir::Library,
    root_ctx: Option<&'a mut Context>,

    warnings: Vec<DigestWarning>,

    namespaces: HashSet<IdentPath>,
}

impl<'a> DigestBuilder<'a> {
    fn digest(&mut self) -> DigestResult<()> {
        for (func_id, func_info) in self.library.metadata.functions() {
            if let Err(err) = self.digest_function(func_id, func_info) {
                self.warnings.push(DigestWarning::InvalidFunc(func_id, err));
            }
        }

        Ok(())
    }

    fn span(&self) -> Span {
        self.digest_span.clone()
    }

    fn digest_tags(&mut self, tags: &[ir::TagInfo]) -> DigestResult<Vec<Tag>> {
        let mut result = Vec::with_capacity(tags.len());
        for tag in tags {
            let tag_type = self.digest_type(&tag.class_id.to_class_ptr_type([]))?;

            let tag_def = self.library.metadata
                .get_struct_def(tag.class_id)
                .ok_or_else(|| DigestError::InvalidData)?;

            let mut ctor_members =  Vec::with_capacity(tag.fields.len());
            for (field_id, field_val) in &tag.fields {
                let member_name = tag_def.fields
                    .get(field_id)
                    .and_then(|field_def| field_def.name.as_ref())
                    .ok_or_else(|| DigestError::InvalidData)?;

                let member_val = self.digest_value(field_val)?;

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

    fn digest_sig(&mut self, sig: &ir::FunctionSig) -> DigestResult<FunctionSig> {
        let result_ty = self.digest_type(&sig.result_type)?;

        let mut params = Vec::with_capacity(sig.param_types.len());
        for param_type in &sig.param_types {
            let (param_type, param_mod) = match param_type {
                ir::Type::TempRef(deref_type) => {
                    (self.digest_type(deref_type)?, Some(FunctionParamMod::Var))
                }

                t => {
                    (self.digest_type(t)?, None)
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

    fn digest_value(&mut self, ir_value: &ir::Value) -> DigestResult<Value> {
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
                let of_type = TypeName::Unspecified(self.digest_type(of_type)?);
                let size_lit = Literal::SizeOf(Box::new(of_type));

                Value::from(ConstValue::literal(size_lit, Primitive::Int32, self.span()))
            }
            ir::Value::Default(of_type) => {
                let of_type = TypeName::Unspecified(self.digest_type(of_type)?);
                let default_lit = Literal::DefaultValue(Box::new(of_type));

                Value::from(ConstValue::literal(default_lit, Primitive::Int32, self.span()))
            }
            ir::Value::Ref(r) => {
                self.digest_ref(r)?
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

    fn digest_ref(&mut self, r: &ir::Ref) -> DigestResult<Value> {
        match r {
            ir::Ref::Global(ir::GlobalRef::Variable(id)) => {
                let var_info = self.library
                    .metadata()
                    .get_variable(*id)
                    .ok_or_else(|| DigestError::InvalidData)?;

                let var_type = self.digest_type(&var_info.r#type)?;

                // there should be no references to unnamed vars in a package's interface
                let Some(name_path) = &var_info.name else {
                    return Err(DigestError::InvalidData);
                };

                let decl = IdentPath::from_parts(name_path.path
                    .iter()
                    .map(|part| Ident::new(part, self.span())));

                let var_value = TypedValue::unit_var(var_type, decl, self.span());
                Ok(Value::from(var_value))
            }

            ir::Ref::Global(ir::GlobalRef::StringLiteral(string_id)) => {
                let string_text = self.library.metadata
                    .get_string(*string_id)
                    .ok_or_else(|| DigestError::InvalidData)?;

                let string_val = ConstValue::string_literal(string_text.clone(), self.span());
                Ok(Value::from(string_val))
            }

            ir::Ref::Global(ir::GlobalRef::StaticTypeInfo(ty)) => {
                let typeinfo_type = self.digest_type(&ty)?;
                let typeinfo_literal = Literal::TypeInfo(Box::new(TypeName::Unspecified(typeinfo_type)));
                let typeinfo_class = Type::class(builtin_typeinfo_name());

                Ok(Value::from(ConstValue::literal(typeinfo_literal, typeinfo_class, self.span())))
            }

            // most ref types should never appear in the interface and can be ignored
            _ => {
                Err(DigestError::InvalidData)
            },
        }
    }

    fn open_unit(&mut self, path: &ir::NamePath) -> DigestResult<ScopeID> {
        assert!(path.type_args.is_empty());

        let unit_path = IdentPath::from_parts(path.path.iter()
            .map(|part| Ident::new(part, self.span())));

        self.namespaces.insert(unit_path.clone());

        if let Some(ctx) = self.root_ctx.as_mut() {
            let scope_id = ctx.push_unit_scope(unit_path)?;
            Ok(scope_id)
        } else {
            Ok(ScopeID(0))
        }
    }
}

pub fn digest(path: &Path, type_ctx: Option<&mut Context>) -> DigestResult<DigestOutput> {
    let mut file = fs::File::open(path)?;

    let mut lib_bytes = Vec::new();
    file.read_to_end(&mut lib_bytes)?;

    let library = decode_lib(&lib_bytes)?;

    let mut builder = DigestBuilder {
        library: &library,
        digest_span: Span::zero(""),
        root_ctx: type_ctx,
        namespaces: HashSet::new(),
        warnings: Vec::new(),
    };

    builder.digest()?;

    Ok(DigestOutput {
        namespaces: builder.namespaces,
        warnings: builder.warnings,
        library,
    })
}
