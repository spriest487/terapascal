use crate::ast::Annotation;
use crate::ast::Expr;
use crate::Operator;
use crate::Position;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Copy, Clone, Eq, Debug, PartialEq, Hash)]
pub enum UnaryPosition {
    Prefix,
    Postfix,
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct UnaryOp<A: Annotation> {
    pub op: Operator,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub op_span: Span,

    pub operand: Expr<A>,

    pub pos: UnaryPosition,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for UnaryOp<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let space_between = match self.op {
            Operator::Not | Operator::As => " ",
            _ => "",
        };
        
        // operators are only valid in either prefix or postfix position, never both
        if self.op.is_valid_in_pos(Position::Prefix) {
            write!(f, "{}{}{}", self.op, space_between, self.operand)
        } else {
            write!(f, "{}{}{}", self.operand, space_between, self.op)
        }
    }
}

impl<A: Annotation> Spanned for UnaryOp<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct BinOp<A: Annotation = Span> {
    pub lhs: Expr<A>,
    
    pub op: Operator,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub op_span: Span,
    
    pub rhs: Expr<A>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for BinOp<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.op {
            Operator::RangeInclusive | Operator::Period => {
                write!(f, "{}{}{}", self.lhs, self.op, self.rhs)
            },

            Operator::Index => {
                write!(f, "{}[{}]", self.lhs, self.rhs)
            },

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
