use crate::ast::operators::CompoundAssignmentOperator;
use crate::ast::Annotation;
use crate::ast::Expr;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct Assignment<A: Annotation> {
    pub lhs: Expr<A>,
    pub rhs: Expr<A>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub op_span: Span,
}

impl<A: Annotation> Spanned for Assignment<A>
where
    A: Spanned,
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for Assignment<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} := {}", self.lhs, self.rhs)
    }
}

#[derive(Eq, Clone, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct CompoundAssignment<A: Annotation> {
    pub lhs: Expr<A>,
    pub rhs: Expr<A>,
    
    pub op: CompoundAssignmentOperator,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub op_span: Span,

    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> Spanned for CompoundAssignment<A>
where
    A: Spanned,
{
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for CompoundAssignment<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}= {}", self.lhs, self.op, self.rhs)
    }
}
