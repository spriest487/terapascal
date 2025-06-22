use terapascal_common::{span::*, TracedError};

use crate::{parse::*, token_tree::*};

pub struct TokenStream {
    tokens: Vec<TokenTree>,
    context: Span,

    position: usize,
}

impl TokenStream {
    pub fn new(tokens: impl IntoIterator<Item = TokenTree>, context: Span) -> Self {
        TokenStream {
            tokens: tokens.into_iter().collect(),
            context,

            position: 0,
        }
    }

    pub fn context(&self) -> &Span {
        &self.context
    }

    pub fn advance(&mut self, count: usize) {
        // skip new stream elements
        for _ in 0..count {
            self.next();
        }
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn current(&self) -> Option<&TokenTree> {
        self.tokens.get(self.position)
    }

    pub fn next(&mut self) -> Option<TokenTree> {
        let tt = self.tokens.get(self.position).cloned()?;
        self.position += 1;
        self.context = tt.span().clone();
        Some(tt)
    }

    pub fn seek(&mut self, position: usize) {
        if position >= self.tokens.len() {
            panic!("seeking token stream of length {} to out-of-range position {}", self.tokens.len(), position);
        }

        self.position = position;
    }

    pub fn finish(mut self) -> ParseResult<()> {
        match self.next() {
            Some(unexpected) => Err(TracedError::trace(ParseError::UnexpectedToken(
                Box::new(unexpected),
                None,
            ))),
            None => Ok(()),
        }
    }

    pub fn match_one(&mut self, matcher: impl Into<Matcher>) -> ParseResult<TokenTree> {
        let matcher = matcher.into();

        match self.next() {
            Some(token) => {
                if matcher.is_match(&token) {
                    Ok(token)
                } else {
                    Err(TracedError::trace(ParseError::UnexpectedToken(
                        Box::new(token),
                        Some(matcher),
                    )))
                }
            }

            None => Err(TracedError::trace(ParseError::UnexpectedEOF(
                matcher,
                self.context.clone(),
            ))),
        }
    }

    /// Match one token in the stream. If the matcher doesn't match the next token, don't
    /// consume it and return `None`. If the matcher does match the next token, consume it and
    /// return it as `Some`.
    pub fn match_one_maybe(&mut self, matcher: impl Into<Matcher>) -> Option<TokenTree> {
        self.look_ahead().match_one(matcher).cloned().map(|tt| {
            self.advance(1);
            tt
        })
    }

    pub fn match_sequence(
        &mut self,
        sequence: impl IntoIterator<Item = Matcher>,
    ) -> ParseResult<Vec<TokenTree>> {
        sequence
            .into_iter()
            .map(|matcher| self.match_one(matcher))
            .collect::<Result<Vec<_>, _>>()
    }

    pub fn look_ahead(&mut self) -> LookAheadTokenStream {
        LookAheadTokenStream {
            tokens: self,
            offset: 0,
        }
    }

    pub fn parse<TParsed>(&mut self) -> ParseResult<TParsed>
    where
        TParsed: Parse,
    {
        TParsed::parse(self)
    }

    pub fn parse_to_end<TParsed>(mut self) -> ParseResult<TParsed>
    where
        TParsed: Parse,
    {
        let result = TParsed::parse(&mut self)?;
        self.finish()?;
        Ok(result)
    }

    pub fn match_repeating<T, F>(&mut self, mut f: F) -> ParseResult<Vec<T>>
    where
        F: FnMut(usize, &mut TokenStream) -> ParseResult<Generate<T>>,
    {
        let mut results = Vec::new();
        loop {
            match f(results.len(), self)? {
                Generate::Yield(result) => results.push(result),
                Generate::Break => break Ok(results),
            }
        }
    }
    
    /// Advance until the next token matches, or the end of the stream is reached.
    /// All non-matching tokens encountered are collected into a list of invalid tokens.
    /// The matched token is not consumed.
    pub fn advance_until(&mut self, matcher: impl Into<Matcher>) -> AdvanceUntilResult {
        let at = self.context().clone();
        
        let matcher = matcher.into();
        let mut invalid = Vec::new();

        let matched = loop {
            match self.look_ahead().next().cloned() {
                Some(tt) if matcher.is_match(&tt) => {
                    break Some(tt);
                }
                
                Some(invalid_tt) => {
                    self.advance(1);

                    invalid.push(invalid_tt);
                }

                None => {
                    break None;
                },
            };
        };

        AdvanceUntilResult {
            at,
            matched,
            matcher,
            invalid,
        }
    }
}

pub struct AdvanceUntilResult {
    pub at: Span,
    pub matcher: Matcher,

    pub matched: Option<TokenTree>,
    pub invalid: Vec<TokenTree>,
}

impl AdvanceUntilResult {
    pub fn to_err(&self) -> Option<TracedError<ParseError>> {
        let unexpected_span = Span::range(&self.invalid)?;

        Some(TracedError::from(ParseError::UnexpectedTokens(
            unexpected_span,
            Some(self.matcher.clone()),
        )))
    }
    
    pub fn and_continue(self, errors: &mut Vec<TracedError<ParseError>>) -> bool {
        if let Some(err) = self.to_err() {
            errors.push(err);
        }
        
        self.matched.is_some()
    }
}

pub trait Parse
where
    Self: Sized,
{
    fn parse(tokens: &mut TokenStream) -> ParseResult<Self>;
}

pub struct LookAheadTokenStream<'tokens> {
    tokens: &'tokens mut TokenStream,

    offset: usize,
}

impl<'tokens> LookAheadTokenStream<'tokens> {
    pub fn next(&mut self) -> Option<&TokenTree> {
        let next_token = self.tokens.tokens.get(self.tokens.position + self.offset)?;
        self.offset += 1;
        Some(next_token)
    }

    pub fn context(&self) -> &Span {
        self.tokens.tokens[self.tokens.position + self.offset].span()
    }

    pub fn match_one(&mut self, matcher: impl Into<Matcher>) -> Option<&TokenTree> {
        let matcher = matcher.into();

        self.next()
            .and_then(|t| if matcher.is_match(&t) { Some(t) } else { None })
    }

    /// check that the next token in the stream matches `matcher` without consuming it, and throw
    /// a ParseError if it doesn't
    pub fn expect_one(&mut self, matcher: impl Into<Matcher>) -> ParseResult<()> {
        let matcher = matcher.into();
        match self.next() {
            Some(ref tt) if matcher.is_match(&tt) => Ok(()),

            Some(unexpected) => Err(TracedError::trace(ParseError::UnexpectedToken(
                Box::new(unexpected.clone()),
                Some(matcher),
            ))),

            None => Err(TracedError::trace(ParseError::UnexpectedEOF(
                matcher,
                self.context().clone(),
            ))),
        }
    }

    pub fn match_sequence(
        &mut self,
        sequence: impl Into<SequenceMatcher>,
    ) -> Option<&[TokenTree]> {
        let mut sequence = sequence.into().into_iter();
        
        let start_offset = self.offset;

        loop {
            let seq_next_matcher = match sequence.next() {
                Some(matcher) => matcher,
                None => {
                    // reached end of sequence without incident
                    let start_pos = self.tokens.position + start_offset;
                    let end_pos = self.tokens.position + self.offset;

                    break Some(&self.tokens.tokens[start_pos..end_pos]);
                }
            };

            match self.next() {
                Some(peeked_token) => {
                    if !seq_next_matcher.is_match(&peeked_token) {
                        break None;
                    }
                }
                None => {
                    // there are fewer tokens in the stream than in the sequence
                    break None;
                }
            };
        }
    }
}

pub enum Generate<T> {
    Break,
    Yield(T),
}
