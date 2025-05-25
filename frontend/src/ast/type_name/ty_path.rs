use crate::ast::IdentPath;
use crate::ast::TypeList;
use crate::parse::ParseResult;
use crate::parse::{Parse, TryParse};
use crate::TokenStream;
use crate::Ident;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use derivative::Derivative;
use std::fmt;
use std::fmt::Formatter;

/// Represents a reference to a type declaration in code, NOT a reference to an instantiation
/// of the type itself: e.g. a path of `A[T]` refers to the type `A` with one parameter, NOT
/// `A` parameterized by the current type `T` in the current scope.
///
/// Used in method declarations e.g. `function A[T].MethodName`
#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct TypePath {
    // todo: nested types
    // if we support nested types in future, this could be either an enum of either a namespace
    // or another TypePath
    pub name: IdentPath,

    pub type_params: Option<TypeList<Ident>>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl Parse for TypePath {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let path = IdentPath::parse(tokens)?;

        let mut span = path.path_span();
        
        let type_params = TypeList::try_parse(tokens)?;
        
        if let Some(type_params) = &type_params {
            span = span.to(type_params.span());
        }

        Ok(TypePath {
            name: path,
            type_params,
            span,
        })
    }
}

impl fmt::Display for TypePath {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(ty_params) = &self.type_params {
            write!(f, "{}", ty_params)?;
        }
        
        Ok(())
    }
}

impl Spanned for TypePath {
    fn span(&self) -> &Span {
        &self.span
    }
}
