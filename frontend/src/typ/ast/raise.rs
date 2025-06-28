use crate::ast;
use crate::ast::SemanticHint;
use crate::typ::ast::typecheck_expr;
use crate::typ::Context;
use crate::typ::Type;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::typ::ValueKind;
use crate::typ::builtin_string_type;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type Raise = ast::Raise<Value>;

pub fn typecheck_raise(
    raise: &ast::Raise<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<Raise> {
    let string_ty = builtin_string_type();
    let value = typecheck_expr(&raise.value, &string_ty, ctx)?;
    value.annotation().expect_value(&string_ty)?;

    // the "raise" expr just aborts, so it has whatever type is expected of it, so we
    // can use it in any expr position
    let annotation = Value::from(TypedValue {
        ty: expect_ty.clone(),
        span: raise.span().clone(),
        decl: None,
        value_kind: ValueKind::Temporary,
        semantic_hint: SemanticHint::None,
    });

    Ok(Raise {
        value: Box::new(value),
        kw_span: raise.kw_span.clone(),
        annotation,
    })
}
