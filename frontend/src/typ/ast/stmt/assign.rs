use crate::ast;
use crate::typ::ast::cast::implicit_conversion;
use crate::typ::ast::evaluate_expr;
use crate::typ::ast::Expr;
use crate::typ::Context;
use crate::typ::Type;
use crate::typ::TypeError;
use crate::typ::TypeResult;
use crate::typ::Value;
use crate::Operator;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

pub type Assignment = ast::Assignment<Value>;
pub type CompoundAssignment = ast::CompoundAssignment<Value>;

pub fn typecheck_assignment(
    assignment: &ast::Assignment<Span>,
    ctx: &mut Context,
) -> TypeResult<Assignment> {
    let (lhs, rhs) = typecheck_operands(&assignment.lhs, &assignment.rhs, ctx)?;

    if let ast::Expr::Ident(ident, ..) = &lhs {
        if ctx.get_decl_scope(ident).is_some() {
            ctx.initialize(ident)
        }
    }

    Ok(Assignment {
        lhs,
        rhs,
        op_span: assignment.op_span.clone(),
        annotation: Value::Untyped(assignment.annotation.clone()),
    })
}

pub fn typecheck_compound_assignment(
    assignment: &ast::CompoundAssignment<Span>,
    ctx: &mut Context,
) -> TypeResult<CompoundAssignment> {
    let (lhs, rhs) = typecheck_operands(&assignment.lhs, &assignment.rhs, ctx)?;

    let lhs_ty = lhs.annotation().ty();
    let rhs_ty = rhs.annotation().ty();

    let result_ty = match lhs_ty.arithmetic_op_result(assignment.op.binary_operator(), &rhs_ty) {
        Some(ty) => ty,
        None => {
            return Err(TypeError::InvalidBinOp {
                lhs: lhs_ty.into_owned(),
                rhs: rhs_ty.into_owned(),
                op: assignment.op.binary_operator(),
                span: assignment.span().clone(),
            });
        }
    };
    
    // compound assignment operator naturally has to return the type of the lhs, since it's
    // being assigned back to the same location
    if *result_ty != *lhs_ty {
        let actual = result_ty.into_owned();
        let expected = lhs_ty.into_owned();
        return Err(TypeError::type_mismatch(expected, actual, assignment.span().clone()));
    }

    Ok(CompoundAssignment {
        lhs,
        rhs,
        annotation: Value::Untyped(assignment.annotation.clone()),
        op: assignment.op,
        op_span: assignment.op_span.clone(),
    })
}

fn typecheck_operands(
    src_lhs: &ast::Expr<Span>,
    src_rhs: &ast::Expr<Span>,
    ctx: &mut Context,
) -> TypeResult<(Expr, Expr)> {
    let lhs = evaluate_expr(&src_lhs, &Type::Nothing, ctx)?;

    // lhs must evaluate to a mutable typed value
    match lhs.annotation() {
        Value::Typed(val) => {
            if !val.value_kind.mutable() {
                return Err(TypeError::NotMutable {
                    decl: val.decl.clone(),
                    expr: Box::new(lhs),
                });
            }
        }
        _ => {
            return Err(TypeError::NotMutable {
                expr: Box::new(lhs),
                decl: None,
            });
        }
    }

    let lhs_ty = lhs.annotation().ty();

    let rhs = evaluate_expr(&src_rhs, &lhs_ty, ctx)?;

    let rhs = implicit_conversion(rhs, &lhs_ty, ctx)
        .map_err(|err| match err {
            TypeError::TypeMismatch {
                expected,
                actual,
                span,
            } => TypeError::InvalidBinOp {
                lhs: expected,
                rhs: actual,
                op: Operator::Assignment,
                span,
            },

            err => err,
        })?;

    Ok((lhs, rhs))
}
