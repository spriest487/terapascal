use crate::ast::Annotation;
use crate::ast::Expr;
use crate::parse::LookAheadTokenStream;
use crate::parse::Matcher;
use crate::parse::ParseError;
use crate::parse::ParseResult;
use crate::parse::ParseSeq;
use crate::parse::TokenStream;
use crate::Keyword;
use crate::Separator;
use crate::TokenTree;
use derivative::*;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::TracedError;

#[derive(Eq, Clone, Derivative)]
#[derivative(PartialEq, Hash, Debug)]
pub enum FunctionDeclMod<A: Annotation = Span> {
    External {
        src: A::ConstStringExpr,

        #[derivative(Hash = "ignore")]
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        kw_span: Span,

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
    
    Published(
        #[derivative(Hash = "ignore")]
        #[derivative(Debug = "ignore")]
        #[derivative(PartialEq = "ignore")]
        Span
    ),
}

impl<A: Annotation> FunctionDeclMod<A> {
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
            FunctionDeclMod::External { .. } => Self::EXTERNAL_WORD,
            FunctionDeclMod::Forward(..) => Self::FORWARD_WORD,
            FunctionDeclMod::Inline(..) => Self::INLINE_WORD,
            FunctionDeclMod::Overload(..) => Self::OVERLOAD_WORD,
            FunctionDeclMod::Published(..) => Keyword::Published.to_str(),
        }
    }

    pub fn keyword_span(&self) -> &Span {
        match self {
            FunctionDeclMod::External { kw_span: span, .. } => span,
            FunctionDeclMod::Forward(span) => span,
            FunctionDeclMod::Inline(span) => span,
            FunctionDeclMod::Overload(span) => span,
            FunctionDeclMod::Published(span) => span,
        }
    }
    
    pub fn arg(&self) -> Option<&A::ConstStringExpr> {
        match self {
            FunctionDeclMod::External { src, .. } => Some(src),
            _ => None,
        }
    }
}

fn word_matcher() -> Matcher {
    Matcher::Ident(String::from(FunctionDeclMod::<Span>::EXTERNAL_WORD))
        | Matcher::Ident(String::from(FunctionDeclMod::<Span>::INLINE_WORD))
        | Matcher::Ident(String::from(FunctionDeclMod::<Span>::FORWARD_WORD))
        | Matcher::Ident(String::from(FunctionDeclMod::<Span>::OVERLOAD_WORD))
        | Keyword::Published
}

impl ParseSeq for FunctionDeclMod<Span> {
    fn parse_group(prev: &[Self], tokens: &mut TokenStream) -> ParseResult<Self> {
        tokens.match_one(Separator::Semicolon)?;

        let word_token = tokens.match_one(word_matcher())?;

        let new_mod = match word_token {
            TokenTree::Ident(ident) => match ident.as_str() {
                Self::EXTERNAL_WORD => {
                    let src = Expr::parse(tokens)?;
                    FunctionDeclMod::External {
                        span: ident.span.to(src.span()),
                        kw_span: ident.span,
                        src: Box::new(src),
                    }
                },

                Self::INLINE_WORD => FunctionDeclMod::Inline(ident.span),
                Self::FORWARD_WORD => FunctionDeclMod::Forward(ident.span),
                Self::OVERLOAD_WORD => FunctionDeclMod::Overload(ident.span),
                
                _ => unreachable!("excluded by matcher"),
            }
            
            tt if tt.is_keyword(Keyword::Published) => {
                FunctionDeclMod::Published(tt.into_span())
            }
            
            _ => unreachable!("excluded by matcher"),
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

        tokens.match_one(word_matcher()).is_some()
    }
}

impl<A: Annotation> fmt::Display for FunctionDeclMod<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionDeclMod::External { src, .. } => write!(f, "{} '{}'", Self::EXTERNAL_WORD, src),
            FunctionDeclMod::Inline(_) => write!(f, "{}", Self::INLINE_WORD),
            FunctionDeclMod::Forward(_) => write!(f, "{}", Self::FORWARD_WORD),
            FunctionDeclMod::Overload(_) => write!(f, "{}", Self::OVERLOAD_WORD),
            FunctionDeclMod::Published(_) => write!(f, "{}", Keyword::Published),
        }
    }
}

impl<A: Annotation> Spanned for FunctionDeclMod<A> {
    fn span(&self) -> &Span {
        match self {
            FunctionDeclMod::External { span, .. } => span,
            FunctionDeclMod::Inline(span) => span,
            FunctionDeclMod::Forward(span) => span,
            FunctionDeclMod::Overload(span) => span,
            FunctionDeclMod::Published(span) => span,
        }
    }
}
