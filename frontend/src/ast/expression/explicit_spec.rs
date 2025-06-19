use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::TypeList;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use derivative::Derivative;
use std::fmt;

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct ExplicitSpecExpr<A = Span> where A: Annotation {
    /// expression, which must refer to a type, these arguments will be applied to
    pub type_expr: Expr<A>,
    
    pub type_args: TypeList<A::TypeName>,
    
    pub annotation: A,
}

impl<A: Annotation> Spanned for ExplicitSpecExpr<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl<A: Annotation> fmt::Display for ExplicitSpecExpr<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} with {}", self.type_expr, self.type_args)
    }
}

impl<A: Annotation> From<ExplicitSpecExpr<A>> for Expr<A> {
    fn from(value: ExplicitSpecExpr<A>) -> Self {
        Expr::ExplicitSpec(Box::new(value))
    }
}
