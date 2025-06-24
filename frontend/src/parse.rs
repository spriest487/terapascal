mod matcher;
mod token_stream;
mod result;

use std::ops::{Deref, DerefMut};
pub use self::matcher::*;
pub use self::result::*;
pub use self::token_stream::*;
use crate::TokenTree;
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
