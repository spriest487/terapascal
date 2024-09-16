use crate::typ::ast::typecheck_expr;
use crate::typ::string_type;
use crate::typ::Context;
use crate::typ::Type;
use crate::typ::Typed;
use crate::typ::TypecheckResult;
use crate::typ::TypedValue;
use crate::typ::ValueKind;
use common::span::Span;
use common::span::Spanned;
use crate::ast;

pub type Raise = ast::Raise<Typed>;

pub fn typecheck_raise(
    raise: &ast::Raise<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypecheckResult<Raise> {
    let string_ty = string_type(ctx)?;
    let value = typecheck_expr(&raise.value, &string_ty, ctx)?;
    value.annotation().expect_value(&string_ty)?;

    // the "raise" expr just aborts, so it has whatever type is expected of it, so we
    // can use it in any expr position
    let annotation = TypedValue {
        ty: expect_ty.clone(),
        span: raise.span().clone(),
        decl: None,
        value_kind: ValueKind::Temporary,
    }
    .into();

    Ok(Raise {
        value: Box::new(value),
        annotation,
    })
}
