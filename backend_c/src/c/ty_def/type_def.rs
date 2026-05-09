use crate::c::boxed::BoxTypeID;
use crate::c::type_map::TypeID;
use crate::c::ArrayTypeID;
use crate::c::DynArrayTypeID;
use crate::c::FuncAliasDef;
use crate::c::StructDef;
use crate::c::TypeDecl;
use crate::c::VariantDef;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum TypeDef {
    Struct(Rc<StructDef>),
    Variant(Rc<VariantDef>),
    FuncAlias(Rc<FuncAliasDef>),
}

impl TypeDef {
    pub fn decl(&self) -> &TypeDecl {
        match self {
            TypeDef::Struct(s) => &s.decl,
            TypeDef::Variant(v) => &v.decl,
            TypeDef::FuncAlias(f) => &f.decl,
        }
    }

    pub fn forward_decl(&self) -> Option<&TypeDecl> {
        match self {
            TypeDef::Struct(s) => Some(&s.decl),
            TypeDef::Variant(v) => Some(&v.decl),
            TypeDef::FuncAlias(..) => None,
        }
    }
}

impl From<StructDef> for TypeDef {
    fn from(value: StructDef) -> Self {
        Self::Struct(Rc::new(value))
    }
}

impl From<Rc<StructDef>> for TypeDef {
    fn from(value: Rc<StructDef>) -> Self {
        Self::Struct(value)
    }
}

impl From<VariantDef> for TypeDef {
    fn from(value: VariantDef) -> Self {
        Self::Variant(Rc::new(value))
    }
}

impl From<Rc<VariantDef>> for TypeDef {
    fn from(value: Rc<VariantDef>) -> Self {
        Self::Variant(value)
    }
}

impl From<FuncAliasDef> for TypeDef {
    fn from(value: FuncAliasDef) -> Self {
        Self::FuncAlias(Rc::new(value))
    }
}

impl From<Rc<FuncAliasDef>> for TypeDef {
    fn from(value: Rc<FuncAliasDef>) -> Self {
        Self::FuncAlias(value)
    }
}

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDef::Struct(s) => write!(f, "{}", s),
            TypeDef::Variant(v) => write!(f, "{}", v),
            TypeDef::FuncAlias(func) => write!(f, "{}", func),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum TypeDefName {
    // struct from class def ID
    Struct(TypeID),

    // struct from variant def ID
    Variant(TypeID),

    // alias for function type
    Alias(TypeID),

    // struct for a fixed-size array with a generated unique ID
    StaticArray(ArrayTypeID),

    // TODO: this shouldn't be in here, move it up to Type
    DynArray(DynArrayTypeID),
    Box(BoxTypeID),
}

impl fmt::Display for TypeDefName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDefName::Struct(id) => write!(f, "Struct_{}", id),
            TypeDefName::Variant(id) => write!(f, "Variant_{}", id),
            TypeDefName::StaticArray(id) => write!(f, "StaticArray_{}", id.0),
            TypeDefName::DynArray(id) => write!(f, "DynArray_{}", id.0),
            TypeDefName::Box(id) => write!(f, "Box_{}", id.0),
            TypeDefName::Alias(id) => write!(f, "FuncAlias_{}", id),
        }
    }
}

impl TypeDefName {
    pub fn build_decl_string(&self, left: &mut String, _right: &mut String) {
        match self {
            // special case some builtins which have nicer names defined in macros
            TypeDefName::Struct(..)
            | TypeDefName::Variant(..)
            | TypeDefName::StaticArray(..)
            | TypeDefName::Box(..)
            | TypeDefName::DynArray(..) => {
                left.push_str(&format!("struct {}", self.to_string()));
            },

            TypeDefName::Alias(..) => {
                left.push_str(&self.to_string());
            },
        }
    }
}
