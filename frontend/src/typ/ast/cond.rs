use crate::ast;
use crate::typ::ast::implicit_conversion;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_stmt;
use crate::typ::ast::Expr;
use crate::typ::ast::Stmt;
use crate::typ::Binding;
use crate::typ::Context;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypePattern;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::typ::TypedValue;
use crate::typ::ValueKind;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use std::borrow::Cow;

pub type IfCond<B> = ast::IfCond<Value, B>;
pub type IfCondExpression = ast::IfCond<Value, Expr>;
pub type IfCondStatement = ast::IfCond<Value, Stmt>;

fn typecheck_cond_expr<B>(
    if_cond: &ast::IfCond<Span, B>,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    // condition expr has a boolean hint if we're not doing an is-match
    let cond_expect_ty = if if_cond.is_pattern.is_some() {
        Type::Nothing
    } else {
        Type::Primitive(Primitive::Boolean)
    };

    let cond = typecheck_expr(&if_cond.cond, &cond_expect_ty, ctx)?;

    // if there's no is-match, implicit conversion of the condition expr to bool
    let cond = match if_cond.is_pattern {
        Some(..) => cond,
        None => implicit_conversion(cond, &Type::Primitive(Primitive::Boolean), ctx)?,
    };

    Ok(cond)
}

fn typecheck_pattern_match<B>(
    if_cond: &ast::IfCond<Span, B>,
    cond: &Expr,
    ctx: &mut Context,
) -> TypeResult<Option<TypePattern>> {
    let is_pattern = match &if_cond.is_pattern {
        Some(pattern) => {
            let pattern = TypePattern::typecheck(pattern, &cond.annotation().ty(), ctx)?;

            Some(pattern)
        },

        None => None,
    };

    Ok(is_pattern)
}

fn create_then_branch_ctx(
    is_pattern: Option<&TypePattern>,
    ctx: &mut Context,
) -> TypeResult<Context> {
    let mut then_ctx = ctx.clone();

    // is-pattern binding only exists in the "then" branch, if present
    if let Some(pattern) = &is_pattern {
        let bindings = pattern
            .bindings(ctx)
            .map_err(|err| TypeError::from_name_err(err, pattern.span().clone()))?;

        for binding in bindings {
            then_ctx.declare_local_var(
                binding.ident.clone(),
                Binding {
                    kind: ValueKind::Immutable,
                    ty: binding.ty.clone(),
                    def: Some(binding.ident.clone()),
                },
            )?;
        }
    }

    Ok(then_ctx)
}

pub fn typecheck_if_cond_stmt(
    if_cond: &ast::IfCond<Span, ast::Stmt<Span>>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<IfCond<Stmt>> {
    let cond = typecheck_cond_expr(&if_cond, ctx)?;

    let is_pattern = typecheck_pattern_match(&if_cond, &cond, ctx)?;

    let mut then_ctx = create_then_branch_ctx(is_pattern.as_ref(), ctx)?;

    let then_branch = typecheck_stmt(&if_cond.then_branch, expect_ty, &mut then_ctx)?;
    let else_branch = match &if_cond.else_branch {
        Some(else_expr) => {
            let mut else_ctx = ctx.clone();
            let else_stmt = typecheck_stmt(else_expr, &Type::Nothing, &mut else_ctx)?;

            ctx.consolidate_branches(&[then_ctx, else_ctx]);
            Some(else_stmt)
        },

        None => {
            ctx.consolidate_branches(&[then_ctx]);
            None
        },
    };

    let annotation = Value::Untyped(if_cond.span().clone());

    Ok(IfCond {
        if_kw_span: if_cond.if_kw_span.clone(),
        cond,
        is_pattern,
        then_kw_span: if_cond.then_kw_span.clone(),
        then_branch,
        else_kw_span: if_cond.else_kw_span.clone(),
        else_branch,
        annotation,
    })
}

pub fn typecheck_if_cond_expr(
    if_cond: &ast::IfCond<Span, ast::Expr<Span>>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<IfCond<Expr>> {
    let cond = typecheck_cond_expr(&if_cond, ctx)?;

    let is_pattern = typecheck_pattern_match(&if_cond, &cond, ctx)?;

    let mut then_ctx = create_then_branch_ctx(is_pattern.as_ref(), ctx)?;

    let then_branch = typecheck_expr(&if_cond.then_branch, expect_ty, &mut then_ctx)?;
    let else_branch = match &if_cond.else_branch {
        Some(else_expr) => {
            let mut else_ctx = ctx.clone();
            let then_ty = then_branch.annotation().ty();

            let else_expr = typecheck_expr(else_expr, &then_ty, &mut else_ctx)?;
            let else_expr = match then_ty.as_ref() {
                Type::Nothing => else_expr,
                then_ty => {
                    then_branch.annotation().expect_value(&Type::Nothing)?;
                    else_expr.annotation().expect_value(&Type::Nothing)?;

                    implicit_conversion(else_expr, &then_ty, ctx)?
                },
            };

            ctx.consolidate_branches(&[then_ctx, else_ctx]);
            Some(else_expr)
        },

        None => {
            ctx.consolidate_branches(&[then_ctx]);
            None
        },
    };

    let span = if_cond.span().clone();

    let annotation = match (then_branch.annotation().ty(), else_branch.as_ref()) {
        (Cow::Owned(Type::Nothing) | Cow::Borrowed(Type::Nothing), _) | (_, None) => {
            Value::Untyped(span)
        },

        (then_ty, Some(_else_branch)) => TypedValue {
            ty: then_ty.into_owned(),
            value_kind: ValueKind::Temporary,
            span,
            decl: None,
        }
        .into(),
    };

    Ok(IfCond {
        if_kw_span: if_cond.if_kw_span.clone(),
        cond,
        is_pattern,
        then_kw_span: if_cond.then_kw_span.clone(),
        then_branch,
        else_kw_span: if_cond.else_kw_span.clone(),
        else_branch,
        annotation,
    })
}
