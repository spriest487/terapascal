use crate::{ast::Expr, parse::prelude::*};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct UnaryOp<A: Annotation> {
    pub op: Operator,
    pub operand: Expr<A>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for UnaryOp<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // operators are only valid in either prefix or postfix position, never both
        if self.op.is_valid_in_pos(Position::Prefix) {
            write!(f, "{}{}", self.op, self.operand)
        } else {
            write!(f, "{}{}", self.operand, self.op)
        }
    }
}

impl<A: Annotation> Spanned for UnaryOp<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct BinOp<A: Annotation> {
    pub lhs: Expr<A>,
    pub op: Operator,
    pub rhs: Expr<A>,
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for BinOp<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.op {
            Operator::RangeInclusive | Operator::Period => {
                write!(f, "{}{}{}", self.lhs, self.op, self.rhs)
            }

            Operator::Index => {
                write!(f, "{}[{}]", self.lhs, self.rhs)
            }

            _ => write!(f, "{} {} {}", self.lhs, self.op, self.rhs),
        }
    }
}

impl<A: Annotation> Spanned for BinOp<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

// #[derive(Clone, Debug, Eq, PartialEq, Hash)]
// pub struct Indexer<A: Annotation> {
//     pub base: Expression<A>,
//     pub index: Expression<A>,
//     pub annotation: A,
// }
//
// impl<A: Annotation> Spanned for Indexer<A> {
//     fn span(&self) -> &Span {
//         self.annotation.span()
//     }
// }
//
// impl<A: Annotation> fmt::Display for Indexer<A> {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "{}[{}]", self.base, self.index)
//     }
// }
