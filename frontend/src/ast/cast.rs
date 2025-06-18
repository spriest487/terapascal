use crate::ast::Annotation;
use crate::ast::Expr;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Cast<A: Annotation> {
    pub expr: Expr<A>,
    pub as_type: A::TypeName,

    pub as_kw: Option<Span>,

    pub annotation: A,
}

impl<A: Annotation> fmt::Display for Cast<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} as {}", self.expr, self.as_type)
    }
}

impl<A: Annotation> Spanned for Cast<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}
