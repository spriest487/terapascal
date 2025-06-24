use crate::ast::Ident;
use crate::typ::ast::EnumDecl;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::FunctionDef;
use crate::typ::ast::InterfaceDecl;
use crate::typ::ast::MethodDecl;
use crate::typ::ast::SetDecl;
use crate::typ::ast::StructDecl;
use crate::typ::ast::Tag;
use crate::typ::ast::VariantDecl;
use crate::typ::Decl;
use crate::typ::FunctionSig;
use std::fmt;
use std::fmt::Formatter;
use std::iter;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Def {
    External(Arc<FunctionDecl>),
    Function(Arc<FunctionDef>),
    Struct(Arc<StructDecl>),
    Interface(Arc<InterfaceDecl>),
    Variant(Arc<VariantDecl>),
    Enum(Arc<EnumDecl>),
    Set(Arc<SetDecl>),
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

    pub fn can_redefine_with(&self, new_def: &Self) -> bool {
        match (self, new_def) {
            (Def::External(decl_a), Def::External(decl_b)) => {
                Self::equivalent_func_decl(decl_a, decl_b)
            },

            (Def::Struct(a), Def::Struct(b)) => {
                a.kind == b.kind
                    && a.name == b.name
                    && a.packed == b.packed
                    && a.implements == b.implements
                    && a.where_clause == b.where_clause
                    && a.forward == b.forward
                    && a.fields().eq(b.fields())
                    && Self::redef_compare_all(a.tags.iter(), b.tags.iter(), Tag::eq)
                    && Self::redef_compare_all(a.methods(), b.methods(), |method_a, method_b| {
                        Self::equivalent_method_decl(method_a, method_b)
                    })
            },

            (Def::Interface(a), Def::Interface(b)) => {
                a.forward == b.forward
                    && a.name == b.name
                    && a.supers == b.supers
                    && a.where_clause == b.where_clause
                    && Self::redef_compare_all(a.tags.iter(), b.tags.iter(), Tag::eq)
                    && Self::redef_compare_all(
                        a.methods.iter(),
                        b.methods.iter(),
                        |a_method, b_method| {
                            Self::equivalent_func_decl(&a_method.decl, &b_method.decl)
                        },
                    )
            },

            (Def::Variant(a), Def::Variant(b)) => {
                a.where_clause == b.where_clause
                    && a.implements == b.implements
                    && a.name == b.name
                    && a.cases == b.cases
                    && Self::redef_compare_all(a.tags.iter(), b.tags.iter(), Tag::eq)
            },

            (Def::Enum(a), Def::Enum(b)) => {
                a == b
            },

            (Def::Set(a), Def::Set(b)) => {
                a == b
            },

            // can't redefine a builtin function that has a builtin definition
            (Def::Function(_), Def::Function(_)) => false,
            
            // non-matching
            _ => false,
        }
    }

    // compare two lists of items for a redef. the redef must contain the original items in the same
    // order, but may have extras afterwards
    fn redef_compare_all<'a, T: 'a, F>(
        a: impl IntoIterator<Item = &'a T>,
        b: impl IntoIterator<Item = &'a T>,
        f: F,
    ) -> bool
    where
        F: Fn(&'a T, &'a T) -> bool,
    {
        let b_items = b.into_iter().map(Some).chain(iter::repeat(None));
        let mut all_items = a.into_iter().zip(b_items);

        all_items.all(|(item_a, maybe_item_b)| {
            maybe_item_b.map(|item_b| f(item_a, item_b)).unwrap_or(false)
        })
    }

    fn equivalent_func_decl(a: &FunctionDecl, b: &FunctionDecl) -> bool {
        a.sig() == b.sig()
            && Self::redef_compare_all(a.tags.iter(), b.tags.iter(), Tag::eq)
            && a.name == b.name
            && a.where_clause == b.where_clause
            && a.mods == b.mods
    }

    fn equivalent_method_decl(a: &MethodDecl, b: &MethodDecl) -> bool {
        a.access == b.access && Self::equivalent_func_decl(&a.func_decl, &b.func_decl)
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
    Sig(Arc<FunctionSig>),
}

impl fmt::Display for DefKey {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            DefKey::Unique => write!(f, "unique"),
            DefKey::Sig(sig) => write!(f, "signature {sig}"),
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

// #[derive(Debug, Clone, Hash, Eq, PartialEq)]
// pub struct FunctionDefKey {
//     name: IdentPath,
//     sig: Arc<FunctionSig>,
// }
