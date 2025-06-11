use crate::ast::MethodOwner;
use crate::typ::ast::OverloadCandidate;
use crate::typ::Context;
use crate::typ::GenericError;
use crate::typ::GenericTarget;
use crate::typ::GenericTypeHint;
use crate::typ::InvocationValue;
use crate::typ::NameError;
use crate::typ::OverloadValue;
use crate::typ::Specializable;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeName;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::VariantCaseValue;
use crate::Ident;
use std::sync::Arc;
use terapascal_common::span::Span;

#[derive(Debug, Clone)]
pub enum VariantTypeMemberValue {
    Case(Value),
    Method(Value),
}

pub fn typecheck_variant_type_member(
    variant_name: &Symbol,
    member_ident: &Ident,
    variant_name_span: &Span,
    ctx: &mut Context,
) -> TypeResult<VariantTypeMemberValue> {
    let span = variant_name_span.to(member_ident);
    
    let variant_def = ctx.find_variant_def(&variant_name.full_path)
        .map_err(|err| TypeError::from_name_err(err, span.clone()))?;

    let case_exists = variant_def.find_case(member_ident).is_some();

    if case_exists {
        let case_val = VariantCaseValue {
            variant_name: Arc::new(variant_name.clone()),
            variant_name_span: variant_name_span.clone(),
            case: member_ident.clone(),
            span,
        };

        Ok(VariantTypeMemberValue::Case(Value::VariantCase(Arc::new(case_val))))
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
                NameError::type_member_not_found(variant_ty, member_ident.clone()),
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
    case_val: &VariantCaseValue,
    expect_ty: &Type,
    ctx: &Context
) -> TypeResult<Option<InvocationValue>> {
    let mut variant_name = case_val.variant_name.clone();

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

                return Err(TypeError::from_generic_err(infer_err, case_val.span.clone()));
            }

            Some(name) => {
                variant_name = Arc::new(name);
            }
        }
    }

    // we don't need to specialize the def, we only need to check if the case has a data arg
    let variant_def = ctx
        .find_variant_def(&variant_name.full_path)
        .map_err(|e| TypeError::from_name_err(e, case_val.span.clone()))?;

    match variant_def.find_case(&case_val.case) {
        None => return Ok(None),

        Some(case) => {
            if case.data.is_some() {
                return Ok(None);
            }
        }
    };

    let variant_ty = Type::Variant(variant_name.clone());
    
    let invoke_variant_ctor = InvocationValue::VariantCtor {
        variant_type: TypeName::Named {
            ty: variant_ty,
            span: case_val.variant_name_span.clone(),
        },
        span: case_val.span.clone(),
        case: case_val.case.clone(),
        arg: None,
    };
    
    Ok(Some(invoke_variant_ctor))
}
