use crate::ast::unit::parse_unit_decl;
use crate::ast::unit::unit_binding_start_matcher;
use crate::ast::unit::unit_func_decl_start_matcher;
use crate::ast::Annotation;
use crate::ast::FunctionDecl;
use crate::ast::FunctionDef;
use crate::ast::TypeDecl;
use crate::ast::UnitBinding;
use crate::ast::UseDecl;
use crate::parse::ContinueParse;
use crate::parse::LookAheadTokenStream;
use crate::parse::{Matcher, Parser};
use crate::DelimiterPair;
use crate::Keyword;
use crate::Separator;
use std::fmt;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Copy, Debug, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub enum Visibility {
    Implementation,
    Interface,
}

impl Visibility {
    pub fn keyword(self) -> Keyword {
        match self {
            Visibility::Implementation => Keyword::Implementation,
            Visibility::Interface => Keyword::Interface,
        }
    }
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.keyword())
    }
}

#[derive(Clone, Debug)]
pub enum UnitDecl<A: Annotation = Span> {
    FunctionDecl { decl: Arc<FunctionDecl<A>> },
    FunctionDef { def: Arc<FunctionDef<A>> },
    Type { decl: TypeDecl<A> },
    Uses { decl: UseDecl },
    Binding { decl: UnitBinding<A> },
}

impl<A: Annotation> Spanned for UnitDecl<A> {
    fn span(&self) -> &Span {
        match self {
            UnitDecl::FunctionDecl {
                decl: func_decl, ..
            } => func_decl.span(),
            UnitDecl::FunctionDef { def: func_def, .. } => func_def.span(),
            UnitDecl::Type {
                decl: type_decl, ..
            } => type_decl.span(),
            UnitDecl::Uses { decl: use_decl } => use_decl.span(),
            UnitDecl::Binding { decl, .. } => decl.span(),
        }
    }
}

impl<A: Annotation> fmt::Display for UnitDecl<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnitDecl::FunctionDecl {
                decl: func_decl, ..
            } => write!(f, "{}", func_decl),
            UnitDecl::FunctionDef { def: func_def, .. } => write!(f, "{}", func_def),
            UnitDecl::Type { decl: ty_decl, .. } => write!(f, "{}", ty_decl),
            UnitDecl::Uses { decl: uses } => write!(f, "{}", uses),
            UnitDecl::Binding { decl, .. } => write!(f, "{}", decl),
        }
    }
}

impl UnitDecl<Span> {
    pub fn start_matcher() -> Matcher {
        Keyword::Uses
            | Keyword::Type
            | unit_func_decl_start_matcher() | unit_binding_start_matcher()
            | DelimiterPair::SquareBracket // tags group before function
    }

    pub fn parse_seq(parser: &mut Parser, visibility: Visibility) -> Vec<Self> {
        let mut items = Vec::new();

        loop {
            if !Self::has_more(&items, &mut parser.tokens().look_ahead()) {
                break;
            }

            if !items.is_empty() {
                if parser.tokens()
                    .advance_to(Separator::Semicolon)
                    .and_continue(parser.errors())
                    .is_none() 
                {
                    break;
                }
            }

            if let Some(item) = parse_unit_decl(parser, visibility)
                .map(Some)
                .or_continue(parser.errors(), None)
            {
                items.push(item);
            }
        }

        items
    }

    pub fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }
        tokens.match_one(UnitDecl::start_matcher()).is_some()
    }
}
