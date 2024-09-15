mod lex;

#[cfg(test)]
mod test;

pub use crate::parse::TokenStream;
use crate::consts::{IntConstant, RealConstant};
use common::{span::*, DiagnosticLabel, DiagnosticOutput, TracedError};
use std::rc::Rc;
use std::fmt::{self, Write as _};
use crate::pp::PreprocessedUnit;
use crate::ast::Ident;
use crate::ast::Keyword;
use crate::ast::Operator;

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum DelimiterPair {
    BeginEnd,
    CaseEnd,
    MatchEnd,
    Bracket,
    SquareBracket,
}

impl DelimiterPair {
    pub fn tokens(self) -> (&'static str, &'static str) {
        match self {
            DelimiterPair::BeginEnd => ("begin", "end"),
            DelimiterPair::CaseEnd => ("case", "end"),
            DelimiterPair::MatchEnd => ("match", "end"),
            DelimiterPair::Bracket => ("(", ")"),
            DelimiterPair::SquareBracket => ("[", "]"),
        }
    }
}

impl fmt::Display for DelimiterPair {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DelimiterPair::BeginEnd => "begin/end",
                DelimiterPair::CaseEnd => "case/end",
                DelimiterPair::MatchEnd => "match/end",
                DelimiterPair::SquareBracket => "[]",
                DelimiterPair::Bracket => "()",
            }
        )
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub enum Separator {
    Semicolon,
    Comma,
    Colon,
}

impl fmt::Display for Separator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Separator::Colon => ':',
                Separator::Comma => ',',
                Separator::Semicolon => ';',
            }
        )
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct DelimitedGroup {
    pub delim: DelimiterPair,

    pub open: Span,
    pub close: Span,

    pub inner: Vec<TokenTree>,

    pub span: Span,
}

impl DelimitedGroup {
    pub fn to_inner_tokens(self) -> TokenStream {
        TokenStream::new(self.inner, self.open)
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub enum TokenTree {
    Ident(Ident),
    IntNumber { value: IntConstant, span: Span },
    RealNumber { value: RealConstant, span: Span },
    String { value: Rc<String>, span: Span },
    Keyword { kw: Keyword, span: Span },
    Operator { op: Operator, span: Span },
    Separator { sep: Separator, span: Span },
    Delimited(DelimitedGroup),
}

impl TokenTree {
    pub fn span(&self) -> &Span {
        match self {
            TokenTree::Ident(ident) => &ident.span,
            TokenTree::IntNumber { span, .. } => span,
            TokenTree::RealNumber { span, .. } => span,
            TokenTree::String { span, .. } => span,
            TokenTree::Keyword { span, .. } => span,
            TokenTree::Operator { span, .. } => span,
            TokenTree::Separator { span, .. } => span,
            TokenTree::Delimited(DelimitedGroup { span, .. }) => span,
        }
    }

    pub fn into_span(self) -> Span {
        match self {
            TokenTree::Ident(ident) => ident.span,
            TokenTree::IntNumber { span, .. } => span,
            TokenTree::RealNumber { span, .. } => span,
            TokenTree::String { span, .. } => span,
            TokenTree::Keyword { span, .. } => span,
            TokenTree::Operator { span, .. } => span,
            TokenTree::Separator { span, .. } => span,
            TokenTree::Delimited(DelimitedGroup { span, .. }) => span,
        }
    }

    pub fn is_ident(&self, word: &str) -> bool {
        match self {
            TokenTree::Ident(ident) => *ident.name == word,
            _ => false,
        }
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        match self {
            TokenTree::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn into_ident(self) -> Option<Ident> {
        match self {
            TokenTree::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn into_delimited_group(self) -> Option<DelimitedGroup> {
        match self {
            TokenTree::Delimited(group) => Some(group),
            _ => None,
        }
    }

    pub fn is_keyword(&self, kw: Keyword) -> bool {
        match self {
            TokenTree::Keyword { kw: token_kw, .. } => *token_kw == kw,
            _ => false,
        }
    }

    pub fn as_keyword(&self) -> Option<Keyword> {
        match self {
            TokenTree::Keyword { kw, .. } => Some(*kw),
            _ => None,
        }
    }

    pub fn is_delimited(&self, delim: DelimiterPair) -> bool {
        match self {
            TokenTree::Delimited(DelimitedGroup {
                delim: token_delim, ..
            }) => *token_delim == delim,
            _ => false,
        }
    }

    pub fn is_separator(&self, sep: Separator) -> bool {
        match self {
            TokenTree::Separator { sep: token_sep, .. } => *token_sep == sep,
            _ => false,
        }
    }

    pub fn is_operator(&self, op: Operator) -> bool {
        match self {
            TokenTree::Operator { op: token_op, .. } => *token_op == op,
            _ => false,
        }
    }

    pub fn as_operator(&self) -> Option<Operator> {
        match self {
            TokenTree::Operator { op, .. } => Some(*op),
            _ => None,
        }
    }

    pub fn as_literal_string(&self) -> Option<&str> {
        match self {
            TokenTree::String { value, .. } => Some(value),
            _ => None,
        }
    }

    pub fn as_literal_int(&self) -> Option<&IntConstant> {
        match self {
            TokenTree::IntNumber { value, .. } => Some(value),
            _ => None,
        }
    }

    pub fn as_literal_real(&self) -> Option<&RealConstant> {
        match self {
            TokenTree::RealNumber { value, .. } => Some(value),
            _ => None,
        }
    }

    fn fmt_indented(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        fn write_indent(f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
            const INDENT_SIZE: usize = 2;
            for _ in 0..indent * INDENT_SIZE {
                f.write_char(' ')?;
            }
            Ok(())
        }

        write_indent(f, indent)?;

        match self {
            TokenTree::Keyword { kw, .. } => write!(f, "keyword `{}`", kw)?,
            TokenTree::Ident(ident) => write!(f, "identifier `{}`", ident)?,
            TokenTree::Operator { op, .. } => write!(f, "operator `{}`", op)?,
            TokenTree::Separator { sep, .. } => write!(f, "separator `{}`", sep)?,

            TokenTree::RealNumber { value, .. } => write!(f, "real number `{}`", value)?,
            TokenTree::IntNumber { value, .. } => write!(f, "integer number `{}`", value)?,
            TokenTree::String { value, .. } => write!(f, "string '{}'", value)?,

            TokenTree::Delimited(DelimitedGroup { delim, .. }) => {
                write!(f, "{}-delimited group", delim)?;
            },
        }

        Ok(())
    }
}

impl fmt::Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_indented(f, 0)
    }
}

#[derive(Debug)]
pub enum TokenizeError {
    IllegalToken(String, Span),
    UnmatchedDelimiter {
        delim: DelimiterPair,
        to_match: Span,
        span: Span,
    },
    UnexpectedCloseDelimited {
        delim: DelimiterPair,
        span: Span,
    },
    UnterminatedStringLiteral(Span),
    IllegalChar(Span),
}

impl fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenizeError::IllegalToken(token, ..) => write!(f, "Illegal token: `{}`", token),

            TokenizeError::UnmatchedDelimiter { .. } => write!(f, "unmatched delimiter"),

            TokenizeError::UnexpectedCloseDelimited { .. } => {
                write!(f, "unexpected close delimiter")
            },

            TokenizeError::UnterminatedStringLiteral(..) => write!(f, "Unterminated string literal"),
            TokenizeError::IllegalChar(..) => write!(f, "Illegal character code"),
        }
    }
}

impl Spanned for TokenizeError {
    fn span(&self) -> &Span {
        match self {
            TokenizeError::IllegalToken(_, span) => span,
            TokenizeError::UnmatchedDelimiter { span, .. } => span,
            TokenizeError::UnexpectedCloseDelimited { span, .. } => span,
            TokenizeError::UnterminatedStringLiteral(span) => span,
            TokenizeError::IllegalChar(span) => span,
        }
    }
}

impl DiagnosticOutput for TokenizeError {
    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            TokenizeError::IllegalToken(_, span) => Some(DiagnosticLabel {
                span: span.clone(),
                text: None,
            }),

            TokenizeError::IllegalChar(span) => Some(DiagnosticLabel {
                span: span.clone(),
                text: None,
            }),

            TokenizeError::UnmatchedDelimiter {
                delim, to_match, ..
            } => Some(DiagnosticLabel {
                span: to_match.clone(),
                text: {
                    let (open, close) = delim.tokens();
                    Some(format!(
                        "opening `{}` is not followed by a closing `{}`",
                        open, close
                    ))
                },
            }),

            TokenizeError::UnexpectedCloseDelimited { delim, span } => Some(DiagnosticLabel {
                span: span.clone(),
                text: {
                    let (open, close) = delim.tokens();
                    Some(format!(
                        "closing `{}` was not expected here (no opening `{}`)",
                        close, open
                    ))
                },
            }),

            TokenizeError::UnterminatedStringLiteral(span) => Some(DiagnosticLabel {
                span: span.clone(),
                text: None,
            })
        }
    }
}

pub type TokenizeResult<T> = Result<T, TracedError<TokenizeError>>;

impl TokenTree {
    pub fn tokenize(unit: PreprocessedUnit) -> TokenizeResult<Vec<Self>> {
        lex::lex(unit)
    }
}
