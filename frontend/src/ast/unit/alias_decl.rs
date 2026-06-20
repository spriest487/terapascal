use crate::ast::type_name::TypeName;
use crate::ast::Annotation;
use crate::ast::DeclIdent;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use derivative::Derivative;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub struct AliasDecl<A: Annotation = Span> {
    pub name: A::DeclName,

    pub target: Box<TypeName<A>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl AliasDecl<Span> {
    pub fn parse(tokens: &mut TokenStream, name: DeclIdent) -> ParseResult<Self> {
        let ty = TypeName::parse(tokens)?;
        
        let mut span = name.span.clone();
        span.extend(&ty);

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
