use crate::ast::VariantCtorCall;
use crate::ast::{Call, MethodOwner};
use crate::typ::ast::{Expr, OverloadCandidate};
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::NameContainer;
use crate::typ::NameError;
use crate::typ::Specializable;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::VariantCaseValue;
use crate::typ::{Context, OverloadValue, Value};
use crate::Ident;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Debug, Clone)]
pub enum VariantTypeMemberValue {
    Case(Value),
    Method(Value),
    Ctor(Expr),
}

pub fn typecheck_variant_type_member(
    variant_name: &Symbol,
    member_ident: &Ident,
    span: &Span,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<VariantTypeMemberValue> {
    let variant_def = ctx.find_variant_def(&variant_name.full_path)
        .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

    let case_exists = variant_def.find_case(member_ident).is_some();

    if case_exists {
        let ctor_annotation = VariantCaseValue {
            variant_name: Arc::new(variant_name.clone()),
            case: member_ident.clone(),
            span: member_ident.span().clone(),
        };

        if let Some(case_ctor) = try_expr_into_noargs_variant_ctor(&ctor_annotation, expect_ty, span, ctx)? {
            // in this context, this becomes a case constructor
            Ok(VariantTypeMemberValue::Ctor(case_ctor))
        } else {
            // reference to the case itself
            Ok(VariantTypeMemberValue::Case(Value::VariantCase(Arc::new(ctor_annotation))))
        }
    } else {
        // must be referencing a method
        // we need the full specialized type in this case
        let variant_def = ctx
            .instantiate_variant_def(&variant_name)
            .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

        let variant_ty = Type::variant(variant_name.clone());

        let method_candidates: Vec<_> = variant_def
            .find_methods(member_ident)
            .map(|(method_index, method_decl)| {
                OverloadCandidate::Method {
                    self_ty: variant_ty.clone(),
                    iface_ty: variant_ty.clone(),
                    decl: method_decl.clone(),
                    index: method_index,
                }
            })
            .collect();
        
        if method_candidates.is_empty() {
            return Err(TypeError::from_name_err(
                NameError::MemberNotFound {
                    base: NameContainer::Type(Type::variant(variant_name.clone())),
                    member: member_ident.clone(),
                },
                span.clone(),
            ));
        }
        
        let known_sig = if method_candidates.len() == 1 {
            Some(Arc::new(method_candidates[0].decl().sig()))
        } else {
            None
        };
        
        Ok(VariantTypeMemberValue::Method(Value::from(OverloadValue {
            candidates: method_candidates,
            span: span.clone(),
            self_arg: None,
            sig: known_sig,
        })))
    }
}

pub fn try_expr_into_noargs_variant_ctor(
    case_annotation: &VariantCaseValue,
    expect_ty: &Type,
    span: &Span,
    ctx: &Context
) -> TypeResult<Option<Expr>> {
    let mut variant_name = case_annotation.variant_name.clone();

    // if the variant is generic, we have to be able to infer the type from the usage
    if variant_name.is_unspecialized_generic() {
        let inferred_name = match expect_ty {
            Type::Variant(expect_name) => {
                variant_name.infer_specialized_from_hint(expect_name).cloned()
            }

            _ => None,
        };

        match inferred_name {
            None => {
                let infer_err = GenericError::CannotInferArgs {
                    target: GenericTarget::Name(variant_name.full_path.clone()),
                    hint: GenericTypeHint::ExpectedValueType(expect_ty.clone()),
                };

                return Err(TypeError::from_generic_err(infer_err, span.clone()));
            }

            Some(name) => {
                variant_name = Arc::new(name);
            }
        }
    }

    // we don't need to specialize the def, we only need to check if the case has a data arg
    let variant_def = ctx
        .find_variant_def(&variant_name.full_path)
        .map_err(|e| TypeError::from_name_err(e, span.clone()))?;

    match variant_def.find_case(&case_annotation.case) {
        None => return Ok(None),

        Some(case) => {
            if case.data_ty.is_some() {
                return Ok(None);
            }
        }
    };

    let variant_ty = Type::Variant(variant_name.clone());

    let ctor_call = VariantCtorCall {
        case: case_annotation.case.clone(),
        variant: variant_name.clone(),
        arg: None,
        annotation: TypedValue::temp(variant_ty, span.clone()).into(),
    };

    let call_expr = Expr::Call(Box::new(Call::VariantCtor(ctor_call)));

    Ok(Some(call_expr))
}
