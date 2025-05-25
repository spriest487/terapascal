use crate::ast::Annotation;
use crate::ast::Expr;
use crate::ast::Ident;
use crate::parse::LookAheadTokenStream;
use crate::parse::Matcher;
use crate::parse::Parse;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::Separator;
use derivative::*;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;
use std::fmt;

#[derive(Eq, Clone, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub enum DeclMod<A: Annotation = Span> {
    External {
        src: A::ConstStringExpr,

        #[derivative(Hash = "ignore")]
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        span: Span,
    },

    Inline(
        #[derivative(Hash = "ignore")]
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        Span
    ),

    Forward(
        #[derivative(Hash = "ignore")]
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        Span
    ),

    Overload(
        #[derivative(Hash = "ignore")]
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        Span
    ),
}

impl<A: Annotation> DeclMod<A> {
    pub const EXTERNAL_WORD: &'static str = "external";
    pub const FORWARD_WORD: &'static str = "forward";
    pub const INLINE_WORD: &'static str = "inline";
    pub const OVERLOAD_WORD: &'static str = "overload";

    pub const RESERVED_WORDS: [&'static str; 4] = [
        Self::EXTERNAL_WORD,
        Self::FORWARD_WORD,
        Self::INLINE_WORD,
        Self::OVERLOAD_WORD,
    ];

    pub fn keyword(&self) -> &str {
        match self {
            DeclMod::External { .. } => Self::EXTERNAL_WORD,
            DeclMod::Forward(..) => Self::FORWARD_WORD,
            DeclMod::Inline(..) => Self::INLINE_WORD,
            DeclMod::Overload(..) => Self::OVERLOAD_WORD,
        }
    }
}

impl ParseSeq for DeclMod<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens.match_one(Separator::Semicolon)?;

        let word_token = Ident::parse(tokens)?;

        let new_mod = match word_token.name.as_str() {
            Self::EXTERNAL_WORD => {
                let src = Expr::parse(tokens)?;
                DeclMod::External {
                    span: word_token.span().to(src.span()),
                    src: Box::new(src),
                }
            },

            Self::INLINE_WORD => DeclMod::Inline(word_token.span().clone()),
            Self::FORWARD_WORD => DeclMod::Forward(word_token.span().clone()),
            Self::OVERLOAD_WORD => DeclMod::Overload(word_token.span().clone()),

            _ => unreachable!("bad modified contextual keyword"),
        };

        let existing = prev.iter().find(|m| m.keyword() == new_mod.keyword());
        if let Some(existing) = existing {
            return Err(TracedError::trace(ParseError::DuplicateModifier {
                new: new_mod,
                existing: existing.clone(),
            }));
        }

        Ok(new_mod)
    }

    fn has_more(_prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }

        let match_any_reserved = Matcher::OneOf(
            Self::RESERVED_WORDS
                .iter()
                .map(|word| Matcher::Ident(word.to_string()))
                .collect(),
        );

        tokens.match_one(match_any_reserved).is_some()
    }
}

impl<A: Annotation> fmt::Display for DeclMod<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DeclMod::External { src, .. } => write!(f, "{} '{}'", Self::EXTERNAL_WORD, src),
            DeclMod::Inline(_) => write!(f, "{}", Self::INLINE_WORD),
            DeclMod::Forward(_) => write!(f, "{}", Self::FORWARD_WORD),
            DeclMod::Overload(_) => write!(f, "{}", Self::OVERLOAD_WORD),
        }
    }
}

impl<A: Annotation> Spanned for DeclMod<A> {
    fn span(&self) -> &Span {
        match self {
            DeclMod::External { span, .. } => span,
            DeclMod::Inline(span) => span,
            DeclMod::Forward(span) => span,
            DeclMod::Overload(span) => span,
        }
    }
}
