use crate::ast::MethodOwner;
use crate::typ::ast::OverloadCandidate;
use crate::typ::overload::OverloadValue;
use crate::typ::Context;
use crate::typ::NameError;
use crate::typ::Symbol;
use crate::typ::Type;
use crate::typ::TypeError;
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
    ctx: &Context,
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
