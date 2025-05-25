use std::fmt;
use std::fmt::Formatter;
use crate::ast::Ident;
use crate::typ::ast::{EnumDecl, SetDecl};
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionDef;
use crate::typ::ast::InterfaceDecl;
use crate::typ::ast::StructDef;
use crate::typ::ast::VariantDef;
use crate::typ::Decl;
use crate::typ::FunctionSig;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Def {
    External(Rc<FunctionDecl>),
    Function(Rc<FunctionDef>),
    Struct(Rc<StructDef>),
    Interface(Rc<InterfaceDecl>),
    Variant(Rc<VariantDef>),
    Enum(Rc<EnumDecl>),
    Set(Rc<SetDecl>),
}

impl Def {
    pub fn ident(&self) -> &Ident {
        match self {
            Def::External(func_decl) => &func_decl.name.ident,
            Def::Function(func_def) => &func_def.decl.name.ident,
            Def::Struct(class) => &class.name.ident(),
            Def::Interface(iface) => &iface.name.ident(),
            Def::Variant(variant) => &variant.name.ident(),
            Def::Enum(enum_decl) => &enum_decl.name.ident(),
            Def::Set(set_decl) => &set_decl.name.ident(),
        }
    }
}

impl Spanned for Def {
    fn span(&self) -> &Span {
        match self {
            Def::External(decl) => decl.span(),
            Def::Function(def) => def.span(),
            Def::Struct(def) => def.span(),
            Def::Interface(def) => def.span(),
            Def::Variant(def) => def.span(),
            Def::Enum(decl) => decl.span(),
            Def::Set(decl) => decl.span(),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum DefKey {
    Unique,
    Sig(Rc<FunctionSig>),
}

impl fmt::Display for DefKey {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            DefKey::Unique => write!(f, "unique"),
            DefKey::Sig(sig) => write!(f, "signature {sig}")
        }
    }
}

// result of comparing a defined name with its previous decl
pub enum DefDeclMatch {
    // the def matches the decl
    Match,

    // the def is the right kind but doesn't match, e.g. function with wrong signature
    Mismatch,

    // the decl with the same name as this def is the wrong kind
    WrongKind,
}

impl DefDeclMatch {
    pub fn always_match(_: &Decl) -> DefDeclMatch {
        DefDeclMatch::Match
    }
}

// 
// #[derive(Debug, Clone, Hash, Eq, PartialEq)]
// pub struct FunctionDefKey {
//     name: IdentPath,
//     sig: Rc<FunctionSig>,
// }
