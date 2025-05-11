use crate::ast::keyword::*;
use crate::ast::operators::*;
use crate::parse::LookAheadTokenStream;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::token_tree::*;
use std::fmt;
use std::mem;
use std::ops::Add;
use std::ops::BitOr;
use std::ops::BitOrAssign;

#[derive(Clone, Debug)]
pub enum Matcher {
    AnyToken,
    
    Keyword(Keyword),
    Operator(Operator),
    Separator(Separator),
    Ident(String),
    Delimited(DelimiterPair),
    AnyKeyword,
    AnyIdent,
    AnyOperator,
    AnyLiteralInteger,
    AnyLiteralString,
    AnyLiteralReal,
    AnyLiteralBoolean,
    LiteralChar,
    Exact(TokenTree),
    OneOf(Vec<Matcher>),

    ExprOperandStart,
    AnyOperatorInPosition(Position),
}

impl From<TokenTree> for Matcher {
    fn from(token: TokenTree) -> Self {
        Matcher::Exact(token)
    }
}

impl From<Operator> for Matcher {
    fn from(op: Operator) -> Self {
        Matcher::Operator(op)
    }
}

impl From<Separator> for Matcher {
    fn from(sep: Separator) -> Self {
        Matcher::Separator(sep)
    }
}

impl From<DelimiterPair> for Matcher {
    fn from(delim: DelimiterPair) -> Self {
        Matcher::Delimited(delim)
    }
}

impl From<Keyword> for Matcher {
    fn from(keyword: Keyword) -> Self {
        Matcher::Keyword(keyword)
    }
}

impl<'a> From<&'a str> for Matcher {
    fn from(name: &str) -> Self {
        Matcher::Ident(name.to_string())
    }
}

pub trait MatchOneOf {
    fn or(self, next: impl Into<Matcher>) -> Matcher;
}

impl<M: Into<Matcher>> MatchOneOf for M {
    fn or(self, next: impl Into<Matcher>) -> Matcher {
        self.into().or(next.into())
    }
}

pub trait MatchSequenceOf {
    fn and_then(self, next: impl Into<Matcher>) -> SequenceMatcher;
}

impl<M: Into<Matcher>> MatchSequenceOf for M {
    fn and_then(self, next: impl Into<Matcher>) -> SequenceMatcher {
        self.into().and_then(next.into())
    }
}

impl<Rhs> BitOr<Rhs> for Matcher where Rhs: Into<Matcher> {
    type Output = Self;

    fn bitor(self, rhs: Rhs) -> Self::Output {
        self.or(rhs.into())
    }
}

impl<Rhs> BitOrAssign<Rhs> for Matcher where Rhs: Into<Matcher> {
    fn bitor_assign(&mut self, rhs: Rhs) {
        let mut temp = Matcher::AnyToken;
        mem::swap(&mut temp, self);
        *self = temp.or(rhs);
    }
}

impl<Rhs> Add<Rhs> for Matcher 
where 
    Rhs: Into<Matcher> 
{
    type Output = SequenceMatcher;

    fn add(self, rhs: Rhs) -> Self::Output {
        self.and_then(rhs.into())
    }
}

impl fmt::Display for Matcher {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Matcher::AnyToken => write!(f, "any token"),
            Matcher::Keyword(kw) => write!(f, "keyword `{}`", kw),
            Matcher::Operator(op) => write!(f, "{}", op),
            Matcher::Separator(sep) => write!(f, "{}", sep),
            Matcher::Ident(name) => f.write_str(name),
            Matcher::Delimited(delim) => write!(f, "{}-delimited group", delim),
            Matcher::AnyKeyword => write!(f, "keyword"),
            Matcher::AnyIdent => write!(f, "identifier"),
            Matcher::AnyOperator => write!(f, "binary operator"),
            Matcher::AnyLiteralInteger => write!(f, "integer literal"),
            Matcher::AnyLiteralString => write!(f, "string literal"),
            Matcher::AnyLiteralReal => write!(f, "floating point literal"),
            Matcher::AnyLiteralBoolean => write!(f, "boolean literal"),
            Matcher::LiteralChar => write!(f, "character"),
            Matcher::Exact(exact_token) => write!(f, "{}", exact_token),
            Matcher::OneOf(matchers) => {
                write!(f, "one of: ")?;
                
                for i in 0..matchers.len() {
                    if i > 0 {
                        write!(f, " or ")?;
                    }
                    write!(f, "{}", matchers[i])?;
                }
                
                Ok(())
            },
            Matcher::ExprOperandStart => {
                write!(f, "start of expression")
            }
            Matcher::AnyOperatorInPosition(pos) => write!(f, "any {} operator", match pos {
                Position::Prefix => "prefix",
                Position::Postfix => "postfix",
                Position::Binary => "infix",
            })
        }
    }
}

impl Matcher {
    pub fn one_of(matchers: impl IntoIterator<Item=Matcher>) -> Self {
        Matcher::OneOf(matchers.into_iter().collect())
    }

    pub fn is_match(&self, token: &TokenTree) -> bool {
        match self {
            Matcher::AnyToken => true,

            Matcher::Separator(sep) => token.is_separator(*sep),
            Matcher::Delimited(delim) => token.is_delimited(*delim),
            Matcher::Keyword(kw) => token.is_keyword(*kw),
            Matcher::AnyKeyword => token.as_keyword().is_some(),
            Matcher::Operator(op) => token.is_operator(*op),
            Matcher::Ident(name) => token.is_ident(name),
            Matcher::AnyIdent => token.as_ident().is_some(),
            Matcher::AnyOperator => token.as_operator().is_some(),
            Matcher::AnyLiteralInteger => token.as_literal_int().is_some(),
            Matcher::AnyLiteralReal => token.as_literal_real().is_some(),
            Matcher::AnyLiteralString => token.as_literal_string().is_some(),
            Matcher::AnyLiteralBoolean => {
                token.is_keyword(Keyword::True) || token.is_keyword(Keyword::False)
            }

            Matcher::LiteralChar => token.as_literal_string()
                .map(|s| s.len() == 1)
                .unwrap_or(false),

            Matcher::Exact(exact_token) => *token == *exact_token,
            Matcher::OneOf(matchers) => matchers.iter().any(|matcher| matcher.is_match(token)),
            Matcher::ExprOperandStart => match token {
                TokenTree::Ident(..)
                | TokenTree::IntNumber { .. }
                | TokenTree::RealNumber { .. }
                | TokenTree::String { .. }
                => true,

                TokenTree::Keyword { kw, .. } => match kw {
                    | Keyword::Unsafe
                    | Keyword::If
                    | Keyword::Raise
                    | Keyword::Exit

                    // inline function decl
                    | Keyword::Function
                    | Keyword::Procedure
                    | Keyword::Lambda

                    // literals
                    | Keyword::Nil
                    | Keyword::SizeOf
                    | Keyword::Default
                    | Keyword::TypeInfo
                    | Keyword::True
                    | Keyword::False
                    => true,

                    _ => false,
                }

                TokenTree::Delimited(DelimitedGroup { delim, .. }) => match delim {
                    // subexpr in brackets or object ctor
                    | DelimiterPair::Bracket

                    // collection constructor
                    | DelimiterPair::SquareBracket

                    // block/control flow
                    | DelimiterPair::BeginEnd
                    | DelimiterPair::CaseEnd
                    | DelimiterPair::MatchEnd
                    => true,
                }

                TokenTree::Operator { op, .. } => {
                    // prefix operator applying to next operand
                    Operator::for_position(Position::Prefix).any(|o| o == *op)
                }

                _ => false,
            }
            Matcher::AnyOperatorInPosition(pos) => {
                Operator::for_position(*pos).any(|o| token.is_operator(o))
            }
        }
    }

    pub fn or(self, or: impl Into<Matcher>) -> Matcher {
        match (self, or.into()) {
            (Matcher::OneOf(mut options), Matcher::OneOf(others)) => {
                options.extend(others);
                Matcher::OneOf(options)
            }

            (Matcher::OneOf(mut options), other) => {
                options.push(other);
                Matcher::OneOf(options)
            }

            (this, Matcher::OneOf(mut others)) => {
                others.insert(0, this);
                Matcher::OneOf(others)
            }

            (this, other) => Matcher::OneOf(vec![this, other]),
        }
    }

    pub fn and_then(self, next_matcher: impl Into<Matcher>) -> SequenceMatcher {
        SequenceMatcher {
            sequence: vec![self, next_matcher.into()],
        }
    }
}

pub trait Matchable {
    fn as_matcher(&self) -> Matcher;
}

impl Matchable for Matcher {
    fn as_matcher(&self) -> Matcher {
        self.clone()
    }
}

impl Matchable for TokenTree {
    fn as_matcher(&self) -> Matcher {
        Matcher::Exact(self.clone())
    }
}

#[derive(Clone, Debug)]
pub struct SequenceMatcher {
    sequence: Vec<Matcher>,
}

impl IntoIterator for SequenceMatcher {
    type Item = Matcher;
    type IntoIter = <Vec<Matcher> as IntoIterator>::IntoIter;

    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        self.sequence.into_iter()
    }
}

impl SequenceMatcher {
    pub fn and_then(mut self, next_matcher: impl Into<Matcher>) -> Self {
        self.sequence.push(next_matcher.into());
        self
    }

    pub fn of_one(matcher: impl Into<Matcher>) -> Self {
        Self { sequence: vec![matcher.into()] }
    }
}

pub trait ParseSeq<T = Self> : Sized {
    fn parse_group(prev: &[T], tokens: &mut TokenStream) -> ParseResult<T>;
    fn has_more(prev: &[T], tokens: &mut LookAheadTokenStream) -> bool;

    fn parse_seq(tokens: &mut TokenStream) -> ParseResult<Vec<T>> {
        let mut results = Vec::new();

        while Self::has_more(&results, &mut tokens.look_ahead()) {
            let group_output = Self::parse_group(&results, tokens)?;
            results.push(group_output);
        }

        Ok(results)
    }
}
