use crate::ast::{Annotation, Expr};
use terapascal_common::span::{Span, Spanned};
use std::fmt;
use crate::ast::operators::CompoundAssignmentOperator;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Assignment<A: Annotation> {
    pub lhs: Expr<A>,
    pub rhs: Expr<A>,
    pub annotation: A,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CompoundAssignment<A: Annotation> {
    pub lhs: Expr<A>,
    pub rhs: Expr<A>,
    pub annotation: A,
    pub op: CompoundAssignmentOperator,
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
