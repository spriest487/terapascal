use crate::typ::ast::BinOp;
use crate::typ::ast::Cast;
use crate::typ::ast::Expr;
use crate::typ::ast::IfCond;
use crate::typ::ast::Literal;
use crate::typ::ast::UnaryOp;
use crate::typ::builtin_string_name;
use crate::typ::Context;
use crate::typ::Type;
use crate::Operator;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::BitXor;
use std::ops::Shl;
use std::ops::Shr;

pub trait ConstEval {
    fn const_eval(&self, ctx: &Context) -> Option<Literal>;
}

impl ConstEval for Literal {
    fn const_eval(&self, _ctx: &Context) -> Option<Literal> {
        Some(self.clone())
    }
}

impl ConstEval for Expr {
    fn const_eval(&self, ctx: &Context) -> Option<Literal> {
        match self {
            Expr::Literal(lit) => Some(lit.literal.clone()),
            Expr::BinOp(bin_op) => bin_op.const_eval(ctx),
            Expr::UnaryOp(op) => op.const_eval(ctx),
            Expr::IfCond(cond) => cond.const_eval(ctx),
            Expr::Cast(cast) => cast.const_eval(ctx),
            _ => None,
        }
    }
}

impl ConstEval for BinOp {
    fn const_eval(&self, ctx: &Context) -> Option<Literal> {
        let lhs = self.lhs.const_eval(ctx)?;
        let rhs = self.rhs.const_eval(ctx)?;

        match self.op {
            Operator::Assignment => None,
            Operator::Equals => lhs.try_eq(rhs),
            Operator::NotEquals => lhs.try_eq(rhs).and_then(Literal::try_negate),
            Operator::Add => lhs.try_add(rhs),
            Operator::Sub => lhs.try_sub(rhs),
            Operator::Mul => lhs.try_mul(rhs),
            Operator::IDiv => lhs.try_div(rhs),
            Operator::FDiv => lhs.try_fdiv(rhs),
            Operator::And => lhs.try_and(rhs),
            Operator::Or => lhs.try_or(rhs),
            Operator::Gt => lhs.try_gt(rhs),
            Operator::Gte => lhs.try_lt(rhs).and_then(Literal::try_negate),
            Operator::Lt => lhs.try_lt(rhs),
            Operator::Lte => lhs.try_gt(rhs).and_then(Literal::try_negate),
            Operator::Caret => lhs.try_bitwise(rhs, u64::bitxor),
            Operator::BitAnd => lhs.try_bitwise(rhs, u64::bitand),
            Operator::BitOr => lhs.try_bitwise(rhs, u64::bitor),
            Operator::Shl => lhs.try_bitwise(rhs, u64::shl),
            Operator::Shr => lhs.try_bitwise(rhs, u64::shr),
            _ => None,
        }
    }
}

impl ConstEval for UnaryOp {
    fn const_eval(&self, ctx: &Context) -> Option<Literal> {
        match self.op {
            Operator::BitNot => {
                self.operand.const_eval(ctx)?.try_bitwise_not()
            }

            Operator::Add => {
                // unary + on a numeric constant just returns itself
                match self.operand.const_eval(ctx)? {
                    Literal::Integer(i) => Some(Literal::Integer(i)),
                    Literal::Real(r) => Some(Literal::Real(r)),

                    _ => None
                }
            }

            Operator::Sub => {
                // unary negation for numeric constants
                self.operand.const_eval(ctx)?.try_negate()
            }

            _ => None,
        }
    }
}

impl ConstEval for IfCond<Expr> {
    fn const_eval(&self, ctx: &Context) -> Option<Literal> {
        let else_branch = self.else_branch.as_ref()?;

        let cond_val = match self.cond.const_eval(ctx)? {
            Literal::Boolean(cond_val) => cond_val,
            _ => return None,
        };

        let then_val = self.then_branch.const_eval(ctx)?;
        let else_val = else_branch.item.const_eval(ctx)?;

        Some(if cond_val {
            then_val
        } else {
            else_val
        })
    }
}

impl ConstEval for Cast {
    fn const_eval(&self, ctx: &Context) -> Option<Literal> {
        let lit_value = self.expr.const_eval(ctx)?;

        match &self.as_type {
            Type::Primitive(primitive) => {
                lit_value.cast_to_primitive(*primitive)
            }
            
            Type::Class(class) => {
                // can only cast to the String class, and can only (redundantly) 
                // cast literal strings to it
                if **class == builtin_string_name() 
                    && *self.expr.annotation().ty().as_ref() == self.as_type {
                    Some(lit_value)
                } else {
                    None
                }
            }
            
            _ => None,
        }
    }
}
