use crate::ast;
use crate::typ::ast::implicit_conversion;
use crate::typ::ast::typecheck_expr;
use crate::typ::ast::typecheck_stmt;
use crate::typ::ast::Expr;
use crate::typ::Context;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::typ::ValueKind;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type CaseBranch<Item> = ast::CaseBranch<Value, Item>;
pub type CaseBlock<Item> = ast::CaseBlock<Value, Item>;
pub type CaseExpr = ast::CaseExpr<Value>;
pub type CaseStmt = ast::CaseStmt<Value>;

pub fn typecheck_case_stmt(
    case: &ast::CaseStmt<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<CaseStmt> {
    let cond_expr = typecheck_expr(&case.cond_expr, &Type::Nothing, ctx)?;
    let cond_ty = cond_expr.annotation().ty();

    let mut branches = Vec::new();
    for branch in &case.branches {
        let case_values = typecheck_case_values(&cond_ty, branch, ctx)?;
        let branch_stmt = typecheck_stmt(&branch.item, &expect_ty, ctx)?;

        branches.push(ast::CaseBranch {
            case_values,
            item: Box::new(branch_stmt),
            span: branch.span.clone(),
        })
    }

    let else_branch = match &case.else_branch {
        Some(else_branch) => {
            let else_stmt = typecheck_stmt(else_branch, &expect_ty, ctx)?;
            Some(else_stmt)
        },

        None => None,
    };

    let annotation = Value::Untyped(case.span().clone());

    Ok(CaseStmt {
        kw_span: case.kw_span.clone(),
        of_span: case.of_span.clone(),
        cond_expr: Box::new(cond_expr),
        branches,
        else_branch: else_branch.map(Box::new),
        annotation,
        end_span: case.end_span.clone(),
    })
}

pub fn typecheck_case_expr(
    case: &ast::CaseExpr<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<CaseExpr> {
    let span = case.span().clone();

    if case.branches.is_empty() || case.else_branch.is_none() {
        return Err(TypeError::InvalidCaseExprBlock { span });
    }

    let cond_expr = typecheck_expr(&case.cond_expr, &Type::Nothing, ctx)?;
    let cond_ty = cond_expr.annotation().ty();
    let mut branches = Vec::new();

    if case.branches.len() == 0 || case.else_branch.is_none() {
        return Err(TypeError::InvalidCaseExprBlock { span });
    }

    let mut branch_expr_ty = None;

    for branch in &case.branches {
        let case_values = typecheck_case_values(&cond_ty, branch, ctx)?;

        let mut branch_expr = typecheck_expr(&branch.item, &expect_ty, ctx)?;

        if let Some(branch_expr_ty) = &branch_expr_ty {
            branch_expr = implicit_conversion(branch_expr, branch_expr_ty, ctx)?;
        } else {
            branch_expr_ty = Some(branch_expr.annotation().ty().into_owned())
        }

        branches.push(ast::CaseBranch {
            case_values,
            item: Box::new(branch_expr),
            span: branch.span.clone(),
        })
    }

    let result_ty = match branch_expr_ty {
        Some(Type::Nothing) | None => {
            return Err(TypeError::InvalidCaseExprBlock { span });
        },

        Some(branch_expr_ty) => branch_expr_ty,
    };

    let else_expr = match &case.else_branch {
        Some(else_branch) => typecheck_expr(else_branch, &result_ty, ctx)?,

        _ => {
            return Err(TypeError::InvalidCaseExprBlock { span });
        },
    };

    let annotation = TypedValue {
        span,
        decl: None,
        value_kind: ValueKind::Temporary,
        ty: result_ty,
    }
    .into();

    Ok(CaseExpr {
        kw_span: case.kw_span.clone(),
        of_span: case.of_span.clone(),
        cond_expr: Box::new(cond_expr),
        branches,
        else_branch: Some(Box::new(else_expr)),
        annotation,
        end_span: case.end_span.clone(),
    })
}

fn typecheck_case_values<Item>(
    cond_ty: &Type,
    branch: &ast::CaseBranch<Span, Item>,
    ctx: &mut Context
) -> TypeResult<Vec<Expr>>{
    branch.case_values
        .iter()
        .map(|expr| {
            let expr = typecheck_expr(expr, &cond_ty, ctx)?;
            expr.annotation().expect_value(&cond_ty)?;
            Ok(expr)
        })
        .collect::<TypeResult<_>>()
}
