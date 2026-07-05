mod matcher;
mod token_stream;
mod result;

pub use self::matcher::*;
pub use self::result::*;
pub use self::token_stream::*;
use crate::Operator;
use crate::TokenTree;

use std::ops::Deref;
use std::ops::DerefMut;
use terapascal_common::ident::Ident;
use terapascal_common::ident::IdentPath;
use terapascal_common::TracedError;

pub struct Parser {
    tokens: TokenStream,
    errors: Vec<TracedError<ParseError>>,
}

impl Deref for Parser {
    type Target = TokenStream;

    fn deref(&self) -> &Self::Target {
        &self.tokens
    }
}

impl DerefMut for Parser {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tokens
    }
}

impl Parser {
    pub fn new(tokens: TokenStream) -> Self {
        Self {
            tokens,
            errors: Vec::new(),
        }
    }
    
    pub fn tokens(&mut self) -> &mut TokenStream {
        &mut self.tokens
    }

    #[inline(always)]
    pub fn error(&mut self, err: TracedError<ParseError>) {
        self.errors.push(err);
    }

    pub fn errors(&mut self) -> &mut Vec<TracedError<ParseError>> {
        &mut self.errors
    }
    
    pub fn finish(mut self) -> Vec<TracedError<ParseError>> {
        if let Err(err) = self.tokens.finish() {
            self.errors.push(err);
        }

        self.errors
    }
    
    pub fn advance_to(&mut self, matcher: impl Into<Matcher>) -> Option<TokenTree> {
        self.tokens.advance_to(matcher).and_continue(&mut self.errors)
    }

    pub fn advance_until(&mut self, matcher: impl Into<Matcher>) -> Option<TokenTree> {
        self.tokens.advance_until(matcher).and_continue(&mut self.errors)
    }
}

pub trait Parse: Sized {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self>;
}

pub trait Match {
    fn is_match(tokens: &mut LookAheadTokenStream) -> bool;
}

pub trait TryParse: Sized {
    fn try_parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>>;
}

impl<T> TryParse for T
where
    T: Parse + Match,
{
    fn try_parse(tokens: &mut TokenStream) -> ParseResult<Option<Self>> {
        if Self::is_match(&mut tokens.look_ahead()) {
            Self::parse(tokens).map(Some)
        } else {
            Ok(None)
        }
    }
}

impl Parse for Ident {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens
            .match_one(Matcher::AnyIdent)
            .map(|tt| tt.into_ident().unwrap())
    }
}

impl Parse for IdentPath {
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self> {
        let path = tokens.match_repeating(|i, tokens| {
            if i > 0 && tokens.match_one_maybe(Operator::Period).is_none() {
                return Ok(Generate::Break);
            }

            let ident_tt = tokens.match_one(Matcher::AnyIdent)?;
            let ident = ident_tt.into_ident().unwrap();
            Ok(Generate::Yield(ident))
        })?;

        assert!(
            !path.is_empty(),
            "parsed ident path must always have 1+ parts"
        );

        Ok(IdentPath::from_parts(path))
    }
}

impl Match for Ident {
    fn is_match(tokens: &mut LookAheadTokenStream) -> bool {
        tokens.match_one(Matcher::AnyIdent).is_some()
    }
}