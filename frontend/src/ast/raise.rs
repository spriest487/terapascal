use crate::ast::Annotation;
use crate::ast::Expr;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Keyword;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct Raise<A: Annotation> {
    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub kw_span: Span,
    
    pub value: Box<Expr<A>>,

    #[derivative(Hash = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    pub annotation: A,
}

impl<A: Annotation> fmt::Display for Raise<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "raise {}", self.value)
    }
}

impl<A: Annotation> Spanned for Raise<A> {
    fn span(&self) -> &Span {
        self.annotation.span()
    }
}

impl Raise<Span> {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let raise_kw = tokens.match_one(Keyword::Raise)?;
        let value = Expr::parse(tokens)?;

        Ok(Self {
            annotation: raise_kw.span().to(value.span()),
            kw_span: raise_kw.into_span(),
            value: Box::new(value),
        })
    }
}