use crate::ast::Annotation;
use crate::ast::BinOp;
use crate::ast::Expr;
use crate::ast::Ident;
use crate::Operator;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;

/// member statements are binary ops with the . operator that may be valid as an expression
/// (where most bin op exprs aren't) because it might be a method/UFCS call. they are syntactically
/// equivalent to a bin op and should be convertible without losing information
#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MemberStmt<A: Annotation = Span> {
    pub base: Box<Expr<A>>,
    pub name: Ident,

    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub op_span: Span,

    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub annotation: A,
}

impl MemberStmt<Span> {
    pub fn into_bin_op(self) -> BinOp {
        let name_span = self.name.span.clone();

        BinOp {
            lhs: *self.base,
            rhs: Expr::Ident(self.name, name_span),
            op: Operator::Period,
            op_span: self.op_span,
            annotation: self.annotation,
        }
    }
}

impl<A: Annotation> fmt::Display for MemberStmt<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.base, self.name)
    }
}
