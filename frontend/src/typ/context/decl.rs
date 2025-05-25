use crate::ast::IdentPath;
use crate::ast::Visibility;
use crate::typ::ast::FunctionDecl;
use crate::typ::ast::Literal;
use crate::typ::builtin_comparable_name;
use crate::typ::builtin_displayable_name;
use crate::typ::builtin_string_name;
use crate::typ::builtin_typeinfo_name;
use crate::typ::Binding;
use crate::typ::DeclConflict;
use crate::typ::FunctionSig;
use crate::typ::Type;
use terapascal_common::span::Span;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Type {
        ty: Type,
        visibility: Visibility,
        forward: bool,
    },

    LocalVariable {
        binding: Binding,
    },
    GlobalVariable {
        binding: Binding,
        visibility: Visibility,
    },
    
    LocalConst {
        ty: Type,
        val: Literal,

        span: Span,
    },
    GlobalConst {
        ty: Type,
        val: Literal,
        visibility: Visibility,
        
        span: Span,
    },

    Function {
        overloads: Vec<DeclFunctionOverload>,
        visibility: Visibility,
    },

    Alias(IdentPath),

    Namespace(IdentPath),
}


impl Decl {
    pub fn visibility(&self) -> Option<Visibility> {
        match self {
            | Decl::Type { visibility, .. }
            | Decl::Function { visibility, .. }
            | Decl::GlobalVariable { visibility, .. }
            | Decl::GlobalConst { visibility, .. } => Some(*visibility),

            | Decl::Alias(_)
            | Decl::Namespace(_)
            | Decl::LocalVariable { .. } => None,
            | Decl::LocalConst { .. } => None,
        }
    }

    /// If no conflict, it's OK to replace `self` with `new_decl` in the same scope.
    /// Handles forward declarations and source code definitions of builtin decls.
    pub fn get_conflict(&self, new_decl: &Decl) -> Option<DeclConflict> {
        if self.visibility() != new_decl.visibility() {
            return Some(DeclConflict::Visibility);
        }

        if new_decl.is_valid_builtin_redecl() {
            return None;
        }

        // forward type declarations can be replaced by any other declaration (forward or not) 
        // as long as the declared type matches exactly
        if let Decl::Type { ty: old_ty, forward: true, ..} = self {
            if let Decl::Type { ty: new_ty, .. } = new_decl {
                return if old_ty == new_ty {
                    None
                } else {
                    Some(DeclConflict::Type)
                }
            }
        }

        Some(DeclConflict::Name)
    }

    // builtin types are OK to redeclare in System.pas
    pub fn is_valid_builtin_redecl(&self) -> bool {
        match self {
            Decl::Type { ty: Type::Interface(iface), .. } => {
                **iface == builtin_displayable_name()
                    || **iface == builtin_comparable_name()
            },

            Decl::Type { ty: Type::Class(sym), .. } => {
                **sym == builtin_string_name() || **sym == builtin_typeinfo_name()
            },

            _ => false,
        }
    }
}

impl fmt::Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Type { ty, .. } => write!(f, "type `{}`", ty),
            
            Decl::LocalConst { ty, val, .. } => {
                write!(f, "const {}: {}", ty, val)
            },
            Decl::GlobalConst { ty, val, visibility, .. } => {
                write!(f, "{} const {}: {}", visibility, ty, val)
            },

            Decl::LocalVariable { binding } => {
                write!(f, "{} of `{}`", binding.kind, binding.ty)
            },
            Decl::GlobalVariable { binding, visibility } => {
                write!(f, "{} {} of `{}`", visibility, binding.kind, binding.ty)
            },

            Decl::Function { overloads, .. } => {
                write!(f, "function group of {} overloads", overloads.len())
            },
            
            Decl::Alias(aliased) => write!(f, "{}", aliased),
            
            Decl::Namespace(namespace) => write!(f, "{}", namespace)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclFunctionOverload {
    decl: Rc<FunctionDecl>,
    
    // cached sig since we access this a lot
    sig: Rc<FunctionSig>,
    
    visibility: Visibility,
}

impl DeclFunctionOverload {
    pub fn new(decl: impl Into<Rc<FunctionDecl>>, visibility: Visibility) -> Self {
        let decl = decl.into();
        let sig = Rc::new(decl.sig());
        
        Self {
            decl,
            sig,
            visibility
        }
    }
    
    pub fn sig(&self) -> &Rc<FunctionSig> {
        &self.sig
    }
    
    pub fn decl(&self) -> &Rc<FunctionDecl> {
        &self.decl
    }
    
    pub fn visiblity(&self) -> Visibility {
        self.visibility
    }
}
