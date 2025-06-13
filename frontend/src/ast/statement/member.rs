use crate::ast::Annotation;
use crate::ast::Expr;
use crate::Ident;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct MemberStmt<A: Annotation = Span> {
    pub base: Box<Expr<A>>,
    pub name: Ident,

    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for MemberStmt<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}", self.base, self.name)
    }
}
