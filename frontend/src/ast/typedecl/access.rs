use crate::parse::Matcher;
use crate::parse::ParseResult;
use crate::Keyword;
use crate::TokenStream;
use crate::TokenTree;
use std::fmt;
use terapascal_common::span::Span;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Ord, PartialOrd, Hash)]
pub enum Access {
    Published,
    Public,
    Private,
}

pub const IFACE_METHOD_ACCESS: Access = Access::Published;

fn unwrap_tt(tt: TokenTree) -> (Access, Span) {
    match tt {
        TokenTree::Keyword { kw: Keyword::Public, span } => (Access::Public, span),
        TokenTree::Keyword { kw: Keyword::Private, span } => (Access::Private, span),
        TokenTree::Keyword { kw: Keyword::Published, span } => (Access::Published, span),

        _ => unreachable!(),
    }
}

fn keyword_matcher() -> Matcher {
    Keyword::Public | Keyword::Private | Keyword::Published
}

impl Access {
    pub fn try_parse(tokens: &mut TokenStream) -> Option<(Self, Span)> {
        match tokens.match_one_maybe(keyword_matcher()) {
            Some(tt) => Some(unwrap_tt(tt)),
            None => None,
        }
    }

    pub fn parse(tokens: &mut TokenStream) -> ParseResult<(Self, Span)> {
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
