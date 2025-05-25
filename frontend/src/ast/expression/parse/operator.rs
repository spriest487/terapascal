use crate::ast::expression::parse::CompoundExpressionPart;
use crate::ast::type_name::TypeName;
use crate::ast::{ArgList, ObjectCtor, ObjectCtorArgs};
use crate::ast::BinOp;
use crate::ast::Call;
use crate::ast::Cast;
use crate::ast::Expr;
use crate::ast::FunctionCall;
use crate::ast::TypeList;
use crate::ast::UnaryOp;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::Operator;
use crate::Position;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;
use std::cmp::Ordering;
use crate::ast::expression::explicit_spec::ExplicitSpecExpr;

fn resolve_postfix<F>(
    parts: Vec<CompoundExpressionPart>,
    lo_op_index: usize,
    span: &Span,
    f: F
) -> ParseResult<Expr<Span>>
    where F: FnOnce(Expr<Span>) -> Expr<Span>
{
    let (before_op, after_op) = parts.split_at(lo_op_index);

    if before_op.is_empty() {
        return Err(TracedError::trace(ParseError::EmptyOperand {
            operator: span.clone(),
            before: true,
        }));
    }

    // everything on the left becomes the operand
    let operand = resolve_ops_by_precedence(before_op.to_vec())?;

    let op_expr = f(operand);

    let merged_parts: Vec<_> = vec![CompoundExpressionPart::Operand(op_expr)]
        .into_iter()
        .chain(after_op[1..].iter().cloned())
        .collect();

    assert!(!merged_parts.is_empty());
    resolve_ops_by_precedence(merged_parts)
}

pub(super) fn resolve_ops_by_precedence(
    parts: Vec<CompoundExpressionPart>
) -> ParseResult<Expr<Span>> {
    assert!(!parts.is_empty(), "expr must not be empty");

    if parts.len() == 1 {
        return Ok(match parts.into_iter().next().unwrap() {
            CompoundExpressionPart::Operand(operand) => operand,
            CompoundExpressionPart::Operator(op_part) => {
                let err = ParseError::UnexpectedOperator {
                    operator: op_part.span(),
                };
                return Err(TracedError::trace(err));
            }
        });
    }

    // find the lowest-precedence operator in the expr, this becomes the
    // outer expr
    let (lo_op_index, lo_op) = parts
        .iter()
        .enumerate()
        .filter_map(|(i, part)| match part {
            CompoundExpressionPart::Operand { .. } => None,
            CompoundExpressionPart::Operator(op) => Some((i, op.clone())),
        })
        .max_by(|(_, op_a), (_, op_b)| op_a.cmp_precedence(op_b))
        .unwrap();

    match lo_op {
        OperatorPart::Call { args, type_args } => {
            let (before_op, after_op) = parts.split_at(lo_op_index);

            if before_op.is_empty() {
                return Err(TracedError::trace(ParseError::EmptyOperand {
                    operator: args.list_span(),
                    before: true,
                }));
            }

            // everything on the left becomes the target
            let target = resolve_ops_by_precedence(before_op.to_vec())?;
            let span = target.annotation().span().to(&args.close);

            let op_expr = Expr::from(Call::Function(FunctionCall {
                annotation: span.clone(),
                args_span: args.list_span(),
                args: args.args,
                target,
                type_args,
            }));

            let merged_parts: Vec<_> = [CompoundExpressionPart::Operand(op_expr)]
                .into_iter()
                .chain(after_op[1..].iter().cloned())
                .collect();

            assert!(!merged_parts.is_empty());
            resolve_ops_by_precedence(merged_parts)
        }

        OperatorPart::ObjectCtor { args, type_args } => {
            let (before_op, after_op) = parts.split_at(lo_op_index);

            let (target, span) = if before_op.is_empty() {
                (None, args.span.clone())
            } else {
                // everything on the left becomes the target, presumably an expression referencing
                // the type to be constructed
                let type_expr = resolve_ops_by_precedence(before_op.to_vec())?;
                let span = type_expr.span().to(&args.span); 
                (Some(type_expr), span)
            };

            let op_expr = Expr::from(ObjectCtor {
                annotation: span,
                type_expr: target,
                type_args,
                args,
            });

            let merged_parts: Vec<_> = [CompoundExpressionPart::Operand(op_expr)]
                .into_iter()
                .chain(after_op[1..].iter().cloned())
                .collect();

            assert!(!merged_parts.is_empty());
            resolve_ops_by_precedence(merged_parts)
        }

        OperatorPart::OperatorSymbol(op_token) => match op_token.pos {
            // merge prefix operator with the operand that follows it
            Position::Binary => {
                let (before_op, after_op) = parts.split_at(lo_op_index);

                if before_op.is_empty() {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.span.clone(),
                        before: true,
                    }));
                }

                // 1 because the op is included in this (?)
                if after_op.len() <= 1 {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.span.clone(),
                        before: false,
                    }));
                }

                let lhs_operand = resolve_ops_by_precedence(Vec::from(before_op))?;
                let rhs_operand =
                    resolve_ops_by_precedence(after_op.iter().skip(1).cloned().collect())?;

                let span = lhs_operand.annotation().to(rhs_operand.annotation());
                let bin_op = BinOp {
                    lhs: lhs_operand,
                    op: op_token.op,
                    rhs: rhs_operand,
                    annotation: span.clone(),
                };

                Ok(Expr::from(bin_op))
            }

            Position::Prefix => {
                let mut before_op = parts;
                let after_op = before_op.split_off(lo_op_index + 1);
                before_op.truncate(lo_op_index);

                if after_op.is_empty() {
                    return Err(TracedError::trace(ParseError::EmptyOperand {
                        operator: op_token.span.clone(),
                        before: false,
                    }));
                }

                let rhs = resolve_ops_by_precedence(after_op)?;

                let op_expr = {
                    let span = op_token.span.to(rhs.annotation().span());
                    let unary_op = UnaryOp {
                        op: op_token.op,
                        operand: rhs,
                        annotation: span.clone(),
                    };
                    Expr::from(unary_op)
                };

                let merged_parts: Vec<_> = before_op
                    .iter()
                    .cloned()
                    .chain(vec![CompoundExpressionPart::Operand(op_expr)])
                    .collect();

                assert!(!merged_parts.is_empty());
                resolve_ops_by_precedence(merged_parts)
            }

            Position::Postfix => {
                resolve_postfix(parts, lo_op_index, &op_token.span, |operand| {
                    let span = op_token.span.to(operand.annotation().span());

                    Expr::from(UnaryOp {
                        op: op_token.op,
                        annotation: span,
                        operand,
                    })
                })
            }
        },

        OperatorPart::AsCast { ty, kw_span } => {
            resolve_postfix(parts, lo_op_index, &kw_span, |operand| {
                let span = operand.span().to(&ty);

                Expr::from(Cast {
                    expr: operand,
                    annotation: span,
                    as_type: ty,
                })
            })
        }
        
        OperatorPart::WithTypeArgs { arg_types, kw_span} => {
            resolve_postfix(parts, lo_op_index, &kw_span, |operand| {
                let span = operand.span().to(&arg_types.span);

                Expr::from(ExplicitSpecExpr {
                    type_expr: operand,
                    type_args: arg_types,
                    annotation: span,
                })
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolOperator {
    pub op: Operator,
    pub pos: Position,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum OperatorPart {
    // symbol operator e.g. +, *
    OperatorSymbol(SymbolOperator),

    // () operator with inner argument list
    Call {
        args: ArgList,
        type_args: Option<TypeList<TypeName>>,
    },

    // () operator with inner field name/value pairs
    ObjectCtor {
        args: ObjectCtorArgs,
        type_args: Option<TypeList<TypeName>>,
    },

    // `as` cast operator followed by typename
    AsCast {
        kw_span: Span,
        ty: TypeName,
    },

    // `with [..]` specialization operator followed by typename list
    WithTypeArgs {
        kw_span: Span,
        arg_types: TypeList<TypeName>,
    },
}

impl OperatorPart {
    pub fn position(&self) -> Position {
        match self {
            OperatorPart::Call { .. } => Position::Postfix,
            OperatorPart::ObjectCtor { .. } => Position::Postfix,
            OperatorPart::OperatorSymbol(sym) => sym.pos,
            OperatorPart::AsCast { .. } => Position::Postfix,
            OperatorPart::WithTypeArgs { .. } => Position::Postfix,
        }
    }

    pub fn op(&self) -> Operator {
        match self {
            // for operator precedence purposes, calls and constructor invocations are the same
            OperatorPart::ObjectCtor { .. } => Operator::Call,
            OperatorPart::Call { .. } => Operator::Call,
            OperatorPart::OperatorSymbol(sym) => sym.op,
            OperatorPart::AsCast { .. } => Operator::As,
            OperatorPart::WithTypeArgs { .. } => Operator::With,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            OperatorPart::OperatorSymbol(sym) => sym.span.clone(),
            
            OperatorPart::Call { args, type_args: Some(ty_args), .. } => {
                ty_args.span().to(&args.close)
            },

            OperatorPart::ObjectCtor { args, .. } => {
                args.span.clone()
            },
            
            OperatorPart::Call { args, .. } => args.open.to(&args.close),
            
            OperatorPart::AsCast { kw_span, ty } => kw_span.to(ty.span()),
            
            OperatorPart::WithTypeArgs { kw_span, arg_types } => {
                kw_span.to(arg_types.span())
            },
        }
    }
}

impl OperatorPart {
    fn cmp_precedence(&self, b: &Self) -> Ordering {
        let op_a = self.op();
        let op_b = b.op();

        let pos_a = self.position();
        let pos_b = b.position();

        let prec_a = op_a.precedence(pos_a);
        let prec_b = op_b.precedence(pos_b);
        prec_a.cmp(&prec_b)
    }
}
