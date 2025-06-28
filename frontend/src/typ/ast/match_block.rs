use crate::ast;
use crate::ast::{ElseBranch, MatchPattern, SemanticHint};
use crate::typ::ast::evaluate_expr;
use crate::typ::ast::implicit_conversion;
use crate::typ::ast::typecheck_stmt;
use crate::typ::ast::Expr;
use crate::typ::ast::Stmt;
use crate::typ::Context;
use crate::typ::Environment;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::TypedValue;
use crate::typ::Value;
use crate::typ::ValueKind;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type MatchBlock<B> = ast::MatchBlock<B, Value>;
pub type MatchExpr = MatchBlock<Expr>;
pub type MatchStmt = MatchBlock<Stmt>;
pub type MatchBlockBranch<B> = ast::MatchBlockBranch<B, Value>;

fn typecheck_match_cond<B>(
    match_block: &ast::MatchBlock<B>,
    ctx: &mut Context,
) -> TypeResult<Expr> {
    let cond_expr = evaluate_expr(&match_block.cond_expr, &Type::Nothing, ctx)?;

    let cond_ty = cond_expr.annotation().ty();
    if !cond_ty.is_matchable() {
        return Err(TypeError::InvalidMatchExpr {
            expr: Box::new(cond_expr),
        });
    }

    Ok(cond_expr)
}

fn typecheck_match_branches<BSrc, B, ItemCheck>(
    match_block: &ast::MatchBlock<BSrc>,
    cond_ty: &Type,
    expect_ty: &Type,
    ctx: &mut Context,
    check_item: ItemCheck,
) -> TypeResult<Vec<MatchBlockBranch<B>>>
where
    ItemCheck: Fn(&BSrc, &Type, &[MatchBlockBranch<B>], &mut Context) -> TypeResult<B>,
    BSrc: Spanned,
{
    if match_block.branches.is_empty() {
        return Err(TypeError::EmptyMatchBlock {
            span: match_block.span().clone(),
        });
    }

    let mut branches = Vec::new();
    let mut branch_ctxs = Vec::new();

    for branch in &match_block.branches {
        let branch_ctx = ctx.branch();

        let branch_env = Environment::Block {
            allow_unsafe: false,
        };

        let branch = ctx.scope(branch_env, |branch_ctx| {
            let pattern = MatchPattern::typecheck(&branch.pattern, cond_ty, branch_ctx)?;

            if let Some(binding) = &branch.binding {
                pattern.declare_binding(binding, branch_ctx)?;
            }

            let item = check_item(&branch.item, expect_ty, &branches, branch_ctx)?;

            Ok(MatchBlockBranch {
                item,
                pattern,
                binding: branch.binding.clone(),
                span: branch.span().clone(),
            })
        })?;

        branches.push(branch);
        branch_ctxs.push(branch_ctx);
    }

    ctx.consolidate_branches(branch_ctxs);

    Ok(branches)
}

pub fn typecheck_match_stmt(
    match_stmt: &ast::MatchStmt<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<MatchStmt> {
    let block_env = Environment::Block {
        allow_unsafe: false,
    };

    ctx.scope(block_env, |block_ctx| {
        let cond_expr = typecheck_match_cond(&match_stmt, block_ctx)?;

        let branches = typecheck_match_branches(
            &match_stmt,
            &cond_expr.annotation().ty(),
            expect_ty,
            block_ctx,
            |item, expect_ty, _branches, ctx| typecheck_stmt(item, expect_ty, ctx),
        )?;

        let else_branch = match &match_stmt.else_branch {
            Some(branch) => {
                let else_stmt = typecheck_stmt(&branch.item, expect_ty, block_ctx)?;
                Some(ElseBranch::new(branch.else_kw_span.clone(), else_stmt))
            },
            None => None,
        };

        let annotation = Value::Untyped(match_stmt.span().clone());

        Ok(MatchStmt {
            kw_span: match_stmt.kw_span.clone(),
            of_span: match_stmt.of_span.clone(),
            cond_expr,
            annotation,
            branches,
            else_branch,
            end_span: match_stmt.end_span.clone(),
        })
    })
}

pub fn typecheck_match_expr(
    match_expr: &ast::MatchExpr<Span>,
    expect_ty: &Type,
    ctx: &mut Context,
) -> TypeResult<MatchExpr> {
    let block_env = Environment::Block {
        allow_unsafe: false,
    };

    ctx.scope(block_env, |block_ctx| {
        let cond_expr = typecheck_match_cond(&match_expr, block_ctx)?;

        let branches = typecheck_match_branches(
            &match_expr,
            &cond_expr.annotation().ty(),
            expect_ty,
            block_ctx,
            |item_expr, expect_ty, branches: &[MatchBlockBranch<Expr>], ctx| {
                if branches.is_empty() {
                    return evaluate_expr(item_expr, expect_ty, ctx);
                }

                let result_ty = branches[0].item.annotation().ty();
                let item_expr = evaluate_expr(item_expr, &result_ty, ctx)?;

                // it's OK for a match expression to result in Nothing (it'll probably need
                // to be converted to a match statement afterward though to continue)
                if *result_ty != Type::Nothing {
                    implicit_conversion(item_expr, &result_ty, ctx)
                } else {
                    item_expr.annotation().expect_no_value()?;
                    Ok(item_expr)
                }
            },
        )?;

        let result_ty = branches[0].item.annotation().ty().into_owned();

        let else_branch = match &match_expr.else_branch {
            Some(branch) => {
                let else_stmt = evaluate_expr(&branch.item, expect_ty, block_ctx)?;
                Some(ElseBranch::new(branch.else_kw_span.clone(), else_stmt))
            },
            None => None,
        };

        if else_branch.is_none() {
            let mut missing_cases = Vec::new();
            let is_exhaustive = match cond_expr.annotation().ty().as_ref() {
                Type::Any | Type::Interface(..) => {
                    // matches on dynamic RC types can never be exhaustive
                    false
                },

                Type::Variant(var_sym) => {
                    let variant_def = block_ctx
                        .find_variant_def(&var_sym.full_path)
                        .map_err(|err| TypeError::from_name_err(err, match_expr.span().clone()))?;

                    // add all variants and remove the ones mentioned by any variant pattern, or
                    // NOT mentioned by any negated variant pattern
                    missing_cases.reserve(variant_def.cases.len());

                    for def_case in &variant_def.cases {
                        let is_mentioned = branches.iter().any(|branch| {
                            let mut result = false;
                            branch.pattern.visit_references(&mut |_, value| {
                                if let Value::VariantCase(case_val) = value {
                                    if case_val.case == def_case.ident {
                                        result = true;
                                    }
                                }
                            });
                            result
                        });

                        if !is_mentioned {
                            missing_cases.push(def_case.ident.clone());
                        }
                    }

                    missing_cases.is_empty()
                },

                _ => true,
            };

            if !is_exhaustive {
                return Err(TypeError::MatchExprNotExhaustive {
                    span: match_expr.span().clone(),
                    missing_cases,
                });
            }
        }

        let annotation = Value::from(TypedValue {
            ty: result_ty,
            span: match_expr.span().clone(),
            decl: None,
            value_kind: ValueKind::Temporary,
            semantic_hint: SemanticHint::None,
        });

        Ok(MatchExpr {
            kw_span: match_expr.kw_span.clone(),
            of_span: match_expr.of_span.clone(),
            cond_expr,
            annotation,
            branches,
            else_branch,
            end_span: match_expr.end_span.clone(),
        })
    })
}
