use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseResult;
use crate::parse::TryParse;
use crate::Keyword;
use crate::TokenStream;
use crate::TokenTree;
use std::fmt;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum Access {
    Published,
    Public,
    Private,
}

pub const IFACE_METHOD_ACCESS: Access = Access::Published;

fn unwrap_tt(tt: TokenTree) -> Access {
    match tt {
        TokenTree::Keyword { kw: Keyword::Public, .. } => Access::Public,
        TokenTree::Keyword { kw: Keyword::Private, .. } => Access::Private,
        TokenTree::Keyword { kw: Keyword::Published, .. } => Access::Published,

        _ => unreachable!(),
    }
}

fn keyword_matcher() -> Matcher {
    Keyword::Public | Keyword::Private | Keyword::Published
}

impl TryParse for Access {
    fn try_parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>> {
        match tokens.match_one_maybe(keyword_matcher()) {
            Some(tt) => Ok(Some(unwrap_tt(tt))),
            None => Ok(None),
        }
    }
}

impl Parse for Access {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let tt = tokens.match_one(keyword_matcher())?;
        Ok(unwrap_tt(tt))
    }
}

impl fmt::Display for Access {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Access::Public => Keyword::Public,
            Access::Private => Keyword::Private,
            Access::Published => Keyword::Published,
        })
    }
} 
