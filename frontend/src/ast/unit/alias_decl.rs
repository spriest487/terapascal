use crate::parse::{Parse, ParseResult};
use crate::{
    ast::{Annotation, DeclIdent},
    parse::TokenStream,
};
use derivative::Derivative;
use terapascal_common::span::{Span, Spanned};
use std::fmt;
use crate::ast::type_name::TypeName;

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct AliasDecl<A: Annotation = Span> {
    pub name: A::DeclName,

    pub target: Box<A::TypeName>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl AliasDecl<Span> {
    pub fn parse(tokens: &mut TokenStream, name: DeclIdent) -> ParseResult<Self> {
        let ty = TypeName::parse(tokens)?;
        
        let mut span = name.span.clone();
        span.maybe_extend(&ty);

        Ok(Self {
            name,
            span,
            target: Box::new(ty),
        })
    }
}

impl<A> Spanned for AliasDecl<A>
where
    A: Annotation,
{
    fn span(&self) -> &Span {
        &self.span
    }
}

impl<A> fmt::Display for AliasDecl<A> where A: Annotation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.target)
    }
}
