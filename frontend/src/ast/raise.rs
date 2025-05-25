use std::fmt;
use terapascal_common::span::{Span, Spanned};
use crate::{
    Keyword,
    ast::{Annotation, Expr},
    parse::{ParseResult, TokenStream}
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Raise<A: Annotation> {
    pub value: Box<Expr<A>>,
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
            value: Box::new(value),
        })
    }
}
