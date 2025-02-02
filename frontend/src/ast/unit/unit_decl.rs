use crate::ast::unit::parse_unit_decl;
use crate::ast::Annotation;
use crate::ast::FunctionDecl;
use crate::ast::FunctionDef;
use crate::ast::TypeDecl;
use crate::ast::UnitBinding;
use crate::ast::UseDecl;
use crate::parse::LookAheadTokenStream;
use crate::parse::MatchOneOf;
use crate::parse::Matcher;
use crate::parse::ParseResult;
use crate::parse::TokenStream;
use crate::Keyword;
use crate::Separator;
use common::span::Span;
use common::span::Spanned;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub enum Visibility {
    Implementation,
    Interface,
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Visibility::Implementation => Keyword::Implementation,
            Visibility::Interface => Keyword::Interface,
        })
    }
}

#[derive(Clone, Debug)]
pub enum UnitDecl<A: Annotation> {
    FunctionDecl { decl: Rc<FunctionDecl<A>> },
    FunctionDef { def: Rc<FunctionDef<A>> },
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
        Keyword::Function
            .or(Keyword::Procedure)
            .or(Keyword::Constructor)
            .or(Keyword::Destructor)
            .or(Keyword::Class)
            .or(Keyword::Uses)
            .or(Keyword::Type)
            .or(Keyword::Const)
            .or(Keyword::Var)
    }

    pub fn parse_seq(part_kw: Keyword, tokens: &mut TokenStream) -> ParseResult<Vec<Self>> {
        let mut items = Vec::new();

        loop {
            if !Self::has_more(&items, &mut tokens.look_ahead()) {
                break;
            }

            if !items.is_empty() {
                tokens.match_one(Separator::Semicolon)?;
            }

            let item = parse_unit_decl(tokens, part_kw)?;
            items.push(item);
        }

        Ok(items)
    }

    pub fn has_more(prev: &[Self], tokens: &mut LookAheadTokenStream) -> bool {
        if !prev.is_empty() && tokens.match_one(Separator::Semicolon).is_none() {
            return false;
        }
        tokens.match_one(UnitDecl::start_matcher()).is_some()
    }
}
