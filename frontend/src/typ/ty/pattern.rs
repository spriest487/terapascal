use crate::ast;
use crate::ast::Ident;
use crate::ast::IdentPath;
use crate::ast::IdentTypeName;
use crate::typ::ast::member_annotation;
use crate::typ::ast::typecheck_type_args;
use crate::typ::result::*;
use crate::typ::ty::Type;
use crate::typ::typecheck_typename;
use crate::typ::Binding;
use crate::typ::Context;
use crate::typ::Specializable;
use crate::typ::TypeName;
use crate::typ::Value;
use crate::typ::VariantCaseValue;
use std::sync::Arc;
use terapascal_common::span::Spanned;

pub type MatchPattern = ast::MatchPattern<Value>;

struct NameMatch {
    pub name: IdentTypeName<Value>,
    pub case: Option<Ident>,
    pub value: Value,
    pub binding_type: Type,
}

impl MatchPattern {
    pub fn typecheck(
        pattern: &ast::MatchPattern,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypeResult<MatchPattern> {
        match pattern {
            ast::MatchPattern::Not {
                pattern,
                not_kw,
                span,
            } => {
                let pattern = Self::typecheck(pattern, &Type::Nothing, ctx)?;

                Ok(MatchPattern::Not {
                    pattern: Box::new(pattern),
                    not_kw: not_kw.clone(),
                    span: span.clone(),
                })
            },

            // this pattern typename will never contain generic args (we can't parse those here),
            // so either this is a non-generic type or we'll infer a specialization from the
            // expr's expected type
            ast::MatchPattern::Name {
                name, annotation, ..
            } => {
                let span = annotation.clone();

                match name {
                    ast::TypeName::Ident(ident_name) if ident_name.is_simple_path() => {
                        let name_match = Self::find_name_match(&ident_name, expect_ty, ctx)?;

                        Ok(MatchPattern::Name {
                            name: TypeName::Ident(name_match.name),
                            annotation: name_match.value,
                            binding_type: name_match.binding_type,
                            case: name_match.case,
                        })
                    },

                    // other typenames must be referring to a specific type
                    _ => {
                        let typename = typecheck_typename(name, ctx)?;
                        let ty = typename.ty().clone();

                        Ok(MatchPattern::Name {
                            binding_type: ty.clone(),
                            annotation: Value::Type(ty, span),
                            name: typename,
                            case: None,
                        })
                    },
                }
            },
        }
    }

    fn find_name_match(
        name: &IdentTypeName,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypeResult<NameMatch> {
        // first try to match the whole path as a type name
        let find_type = ctx
            .find_path(&name.ident)
            .map(|member| member_annotation(&member, name.span.clone(), ctx));

        if let Some(Value::Type(ty, ty_span)) = find_type {
            let (ty, type_args) = match &name.type_args {
                None => {
                    if ty.is_unspecialized_generic() {
                        let spec_ty = ty
                            .infer_specialized_from_hint(expect_ty)
                            .cloned()
                            .ok_or_else(|| TypeError::UnableToInferSpecialization {
                                generic_ty: ty.clone(),
                                hint_ty: expect_ty.clone(),
                                span: name.span.clone(),
                            })?;

                        (spec_ty, None)
                    } else {
                        (ty, None)
                    }
                },

                Some(args) => {
                    let args = typecheck_type_args(args, ctx)?;
                    let ty = ty
                        .specialize(&args, ctx)
                        .map_err(|err| TypeError::from_generic_err(err, name.span.clone()))?
                        .into_owned();

                    (ty, Some(args))
                },
            };

            let value = Value::Type(ty.clone(), ty_span);

            let name = IdentTypeName {
                ident: name.ident.clone(),
                type_args,
                ty: ty.clone(),
                span: name.span.clone(),
                indirection: name.indirection,
            };

            return Ok(NameMatch {
                name,
                value,
                binding_type: ty,
                case: None,
            });
        }

        // second, try to match the path up to the last part as a variant name
        if name.ident.len() < 2 || name.type_args.is_some() || name.indirection != 0 {
            // can only match a typename
            return Err(TypeError::name_not_found(
                name.ident.clone(),
                name.span.clone(),
            ));
        }

        Self::find_variant_case_match(name, expect_ty, ctx)
    }

    fn find_variant_case_match(
        name: &IdentTypeName,
        expect_ty: &Type,
        ctx: &mut Context,
    ) -> TypeResult<NameMatch> {
        let variant_name_parts = name.ident.as_slice()[0..name.ident.len() - 1].to_vec();
        let variant_name = IdentPath::from_vec(variant_name_parts);
        let variant_name_span = variant_name.path_span();

        let find_variant = ctx
            .find_path(&variant_name)
            .map(|member| member_annotation(&member, name.span.clone(), ctx));

        let Some(Value::Type(Type::Variant(variant_def_name), ..)) = find_variant else {
            return Err(TypeError::name_not_found(
                name.ident.clone(),
                name.span.clone(),
            ));
        };

        let mut case_val = VariantCaseValue {
            variant_name_span: variant_name_span.clone(),
            variant_name: variant_def_name,
            case: name.ident.last().clone(),
            span: name.span.clone(),
        };

        if case_val.variant_name.is_unspecialized_generic() {
            case_val.variant_name = expect_ty
                .as_variant()
                .ok()
                .and_then(|variant_name| {
                    case_val
                        .variant_name
                        .infer_specialized_from_hint(variant_name)
                        .cloned()
                })
                .map(Arc::new)
                .ok_or_else(|| TypeError::UnableToInferSpecialization {
                    generic_ty: Type::variant(case_val.variant_name.clone()),
                    hint_ty: expect_ty.clone(),
                    span: name.span.clone(),
                })?;
        }

        let variant_def = ctx
            .instantiate_variant_def(&case_val.variant_name)
            .map_err(|err| TypeError::from_name_err(err, name.span.clone()))?;

        let Some(case) = variant_def.find_case(name.ident.last().as_str()) else {
            return Err(TypeError::name_not_found(
                name.ident.clone(),
                name.span.clone(),
            ));
        };

        let binding_type = case
            .data
            .as_ref()
            .map(|data| data.ty.ty())
            .cloned()
            .unwrap_or(Type::Nothing);

        Ok(NameMatch {
            name: IdentTypeName {
                ident: variant_name,
                indirection: 0,
                span: variant_name_span,
                type_args: None,
                ty: Type::variant(case_val.variant_name.clone()),
            },
            value: Value::VariantCase(Arc::new(case_val)),
            binding_type,
            case: Some(name.ident.last().clone()),
        })
    }

    pub fn binding_type(&self) -> Option<&Type> {
        match self {
            MatchPattern::Name { binding_type, .. } => Some(binding_type),
            MatchPattern::Not { .. } => None,
        }
    }

    pub fn declare_binding(&self, binding_name: &Ident, ctx: &mut Context) -> TypeResult<()> {
        let Some(binding_ty) = self.binding_type() else {
            return Err(TypeError::InvalidPatternBinding {
                pattern: self.clone(),
                span: self.span().clone(),
            });
        };

        let binding = Binding::pattern_binding(binding_name.clone(), binding_ty.clone());

        // doesn't need to be initialized because pattern bindings are always immutable
        ctx.declare_local_var(binding_name.clone(), binding)?;

        Ok(())
    }
}
