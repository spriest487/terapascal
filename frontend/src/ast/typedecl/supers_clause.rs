use crate::ast::Annotation;
use crate::ast::TypeName;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::Keyword;
use crate::Separator;
use crate::TokenStream;
use derivative::Derivative;
use terapascal_common::span::{Span, Spanned};

#[derive(Clone, Eq, Derivative)]
#[derivative(Debug, PartialEq, Hash)]
pub struct SupersClause<A: Annotation = Span> {
    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub kw_span: Span,

    pub types: Vec<A::TypeName>,

    #[derivative(Debug = "ignore")]
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl SupersClause {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>> {
        let Some(of_tt) = tokens.match_one_maybe(Keyword::Of) else {
            return Ok(None);
        };

        let mut types = Vec::new();
        loop {
            let implement_iface = TypeName::parse(tokens)?;

            types.push(implement_iface);

            if tokens.match_one_maybe(Separator::Comma).is_none() {
                break;
            }
        }

        Ok(Some(SupersClause {
            span: of_tt.span().to(types.last().unwrap().span()),
            kw_span: of_tt.into_span(),
            types,
        }))
    }
}
