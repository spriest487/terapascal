use crate::{
    parse::{LookAheadTokenStream, Matcher, ParseError, ParseResult, ParseSeq, TokenStream},
    IdentPath, Keyword, Separator,
};
use pas_common::{
    span::{Span, Spanned},
    TracedError,
};
use std::fmt;
use derivative::*;

#[derive(Clone, Debug)]
pub struct UseDecl {
    pub units: Vec<UseDeclItem>,
    pub span: Span,
}

impl Spanned for UseDecl {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for UseDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "uses ")?;
        for (i, item) in self.units.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }

            write!(f, "{}", item)?;
        }
        Ok(())
    }
}

impl UseDecl {
    pub fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let kw = tokens.match_one(Keyword::Uses)?;

        let items = UseDeclItem::parse_seq(tokens)?;

        let span = match items.last() {
            None => {
                return Err(TracedError::trace(match tokens.look_ahead().next() {
                    None => ParseError::UnexpectedEOF(Matcher::AnyIdent, kw.span().clone()),
                    Some(x) => ParseError::UnexpectedToken(Box::new(x), Some(Matcher::AnyIdent)),
                }));
            },

            Some(last_item) => kw.span().to(last_item.span()),
        };

        Ok(Self { units: items, span })
    }
}

#[derive(Clone, Eq, Derivative)]
#[derivative(PartialEq, Debug, Hash)]
pub struct UseDeclItem {
    pub ident: IdentPath,

    pub path: Option<String>,

    #[derivative(PartialEq = "ignore")]
    #[derivative(Debug = "ignore")]
    #[derivative(Hash = "ignore")]
    pub span: Span,
}

impl Spanned for UseDeclItem {
    fn span(&self) -> &Span {
        &self.span
    }
}

impl fmt::Display for UseDeclItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.ident)?;

        if let Some(path) = &self.path {
            write!(f, " in '{}'", path)?;
        }

        Ok(())
    }
}

impl ParseSeq for UseDeclItem {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        if !prev.is_empty() {
            tokens.match_one(Separator::Comma)?;
        }

        let unit = IdentPath::parse(tokens)?;

        let (path, span) = match tokens.match_one_maybe(Keyword::In) {
            Some(..) => {
                let path_tt = tokens.match_one(Matcher::AnyLiteralString)?;
                let path_string = path_tt.as_literal_string().unwrap().to_string();

                (Some(path_string), unit.span().to(path_tt.span()))
            },

            None => {
                let path_string = None;
                (path_string, unit.span().clone())
            },
        };

        Ok(UseDeclItem {
            ident: unit,
            path,
            span,
        })
    }

    fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Comma).is_none() {
            return false;
        }

        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}
