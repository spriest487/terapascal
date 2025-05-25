use crate::ast::FuncAliasDef;
use crate::ast::StructDef;
use crate::ast::TypeDecl;
use crate::ast::VariantDef;
use crate::ir;
use std::fmt;

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum TypeDef {
    Struct(StructDef),
    Variant(VariantDef),
    FuncAlias(FuncAliasDef),
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

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDef::Struct(s) => write!(f, "{}", s),
            TypeDef::Variant(v) => write!(f, "{}", v),
            TypeDef::FuncAlias(func) => write!(f, "{}", func),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeDefName {
    // struct from class def ID
    Struct(ir::TypeDefID),

    // struct from variant def ID
    Variant(ir::TypeDefID),

    // alias for another type
    Alias(ir::TypeDefID),

    // struct for a fixed-size array with a generated unique ID
    StaticArray(usize),
}

impl fmt::Display for TypeDefName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDefName::Struct(id) => write!(f, "Struct_{}", id.0),
            TypeDefName::Variant(id) => write!(f, "Variant_{}", id.0),
            TypeDefName::StaticArray(i) => write!(f, "StaticArray_{}", i),
            TypeDefName::Alias(id) => write!(f, "FuncAlias_{}", id.0),
        }
    }
}

impl TypeDefName {
    pub fn build_decl_string(&self, left: &mut String, _right: &mut String) {
        match self {
            TypeDefName::Struct(..) | TypeDefName::Variant(..) | TypeDefName::StaticArray(..) => {
                left.push_str(&format!("struct {}", self.to_string()));
            },

            TypeDefName::Alias(..) => {
                left.push_str(&self.to_string());
            },
        }
    }
}
