use crate::ast;
use crate::ast::ElseBranch;
use crate::typ::ast::typecheck_stmt;
use crate::typ::ast::Expr;
use crate::typ::ast::Stmt;
use crate::typ::ast::{evaluate_expr, implicit_conversion};
use crate::typ::Binding;
use crate::typ::Context;
use crate::typ::Primitive;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypePattern;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::typ::ValueKind;
use std::borrow::Cow;
use terapascal_common::span::Spanned;

pub type IfCond<B> = ast::IfCond<B, Value>;
pub type IfCondExpr = IfCond<Expr>;
pub type IfCondStmt = IfCond<Stmt>;

fn typecheck_cond_expr<B>(
    if_cond: &ast::IfCond<B>,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    // condition expr has a boolean hint if we're not doing an is-match
    let cond_expect_ty = if if_cond.is_pattern.is_some() {
        Type::Nothing
    } else {
        Type::Primitive(Primitive::Boolean)
    };

    let cond = evaluate_expr(&if_cond.cond, &cond_expect_ty, ctx)?;

    // if there's no is-match, implicit conversion of the condition expr to bool
    let cond = match if_cond.is_pattern {
        Some(..) => cond,
        None => implicit_conversion(cond, &Type::Primitive(Primitive::Boolean), ctx)?,
    };

    Ok(cond)
}

fn typecheck_pattern_match<B>(
    if_cond: &ast::IfCond<B>,
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
    if_cond: &ast::IfCond<ast::Stmt>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<IfCond<Stmt>> {
    let cond = typecheck_cond_expr(&if_cond, ctx)?;

    let is_pattern = typecheck_pattern_match(&if_cond, &cond, ctx)?;

    let mut then_ctx = create_then_branch_ctx(is_pattern.as_ref(), ctx)?;

    let then_branch = typecheck_stmt(&if_cond.then_branch, expect_ty, &mut then_ctx)?;
    let else_branch = match &if_cond.else_branch {
        Some(branch) => {
            let mut else_ctx = ctx.clone();
            let else_stmt = typecheck_stmt(&branch.item, &Type::Nothing, &mut else_ctx)?;

            ctx.consolidate_branches(&[then_ctx, else_ctx]);
            Some(ElseBranch {
                item: Box::new(else_stmt),
                else_kw_span: branch.else_kw_span.clone(),
            })
        }

        None => {
            ctx.consolidate_branches(&[then_ctx]);
            None
        },
    };

    let annotation = Value::Untyped(if_cond.span().clone());

    Ok(IfCond {
        if_kw_span: if_cond.if_kw_span.clone(),
        cond,
        is_kw: if_cond.is_kw.clone(),
        is_pattern,
        then_kw_span: if_cond.then_kw_span.clone(),
        then_branch,
        else_branch,
        annotation,
    })
}

pub fn typecheck_if_cond_expr(
    if_cond: &ast::IfCond<ast::Expr>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<IfCond<Expr>> {
    let cond = typecheck_cond_expr(&if_cond, ctx)?;

    let is_pattern = typecheck_pattern_match(&if_cond, &cond, ctx)?;

    let mut then_ctx = create_then_branch_ctx(is_pattern.as_ref(), ctx)?;

    let then_branch = evaluate_expr(&if_cond.then_branch, expect_ty, &mut then_ctx)?;

    let else_branch = match &if_cond.else_branch {
        Some(branch) => {
            let mut else_ctx = ctx.clone();
            let then_ty = then_branch.annotation().ty();

            let else_expr = evaluate_expr(&branch.item, &then_ty, &mut else_ctx)?;

            let else_expr = match then_ty.as_ref() {
                Type::Nothing => {
                    else_expr
                },

                then_ty => {
                    then_branch.annotation().expect_any_value()?;
                    else_expr.annotation().expect_any_value()?;

                    implicit_conversion(else_expr, &then_ty, ctx)?
                },
            };

            ctx.consolidate_branches(&[then_ctx, else_ctx]);

            Some(ElseBranch {
                item: Box::new(else_expr),
                else_kw_span: branch.else_kw_span.clone(),
            })
        }

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
        is_kw: if_cond.is_kw.clone(),
        is_pattern,
        then_kw_span: if_cond.then_kw_span.clone(),
        then_branch,
        else_branch,
        annotation,
    })
}
