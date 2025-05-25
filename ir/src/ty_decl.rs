mod r#struct;
mod variant;
mod interface;
mod tags;

use crate::FunctionSig;
use crate::NamePath;
use crate::Type;
use crate::FunctionID;
pub use interface::*;
pub use r#struct::*;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Write;
pub use variant::*;
pub use tags::*;

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct TypeDefID(pub usize);

impl fmt::Display for TypeDefID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct InterfaceID(pub usize);

// key used for distinguishing unique set types, which are implemented using the same underlying
// struct type but can have different RTTI
#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct SetAliasID(pub usize);

impl fmt::Display for SetAliasID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum StructIdentity {
    Record(NamePath),
    Class(NamePath),
    Array(Type, usize),
    DynArray(Type),
    Closure(ClosureIdentity),
    
    // bitmask type for set flag collections. value is the number of bits
    SetFlags { bits: usize }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ClosureIdentity {
    /// the type of the closure's virtual call function alias, which has the sig of the closure's
    /// target type plus a type-erased pointer inserted as parameter 0
    pub virt_func_ty: TypeDefID,

    pub id: FunctionID,
}

impl StructIdentity {
    pub fn is_ref_type(&self) -> bool {
        match self {
            StructIdentity::Array(..) 
            | StructIdentity::Record(..) 
            | StructIdentity::SetFlags { .. } => false,

            StructIdentity::Class(..)
            | StructIdentity::DynArray(..)
            | StructIdentity::Closure(..) => true,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TypeDecl {
    Reserved,
    Forward(NamePath),
    Def(TypeDef),
}

impl TypeDecl {
    pub fn name(&self) -> Option<&NamePath> {
        match self {
            TypeDecl::Reserved => None,
            TypeDecl::Forward(name) => Some(name),
            TypeDecl::Def(def) => def.name(),
        }
    }

    pub fn is_forward(&self) -> bool {
        !matches!(self, TypeDecl::Def(..))
    }
}

impl fmt::Display for TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDecl::Reserved => write!(f, "reserved type ID"),
            TypeDecl::Forward(name) => write!(f, "forward decl of `{}`", name),
            TypeDecl::Def(def) => write!(f, "defined type `{}`", def),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TypeDef {
    Struct(Struct),
    Variant(VariantDef),
    Function(FunctionSig),
}

impl TypeDef {
    pub fn name(&self) -> Option<&NamePath> {
        match self {
            TypeDef::Struct(s) => s.name(),
            TypeDef::Variant(v) => Some(&v.name),
            TypeDef::Function(..) => None,
        }
    }
    
    pub fn to_pretty_string<'a, TyFormat>(&self, ty_format: TyFormat) -> String
    where
        TyFormat: Fn(&Type) -> Cow<'a, str>
    {
        match self {
            TypeDef::Struct(def) => match &def.identity {
                StructIdentity::Class(name) | StructIdentity::Record(name) => {
                    name.to_pretty_string(ty_format)
                },
                StructIdentity::Closure(identity) => {
                    let func_ty_name = ty_format(&Type::Function(identity.virt_func_ty));
                    format!("closure of {} ({})", func_ty_name, identity.id)
                },
                StructIdentity::Array(ty, dim) => {
                    let ty_name = ty_format(ty);
                    format!("array[{}] of {}", dim, ty_name)
                }
                StructIdentity::DynArray(ty) => {
                    let ty_name = ty_format(ty);
                    format!("array of {}", ty_name)
                }
                StructIdentity::SetFlags { bits } => {
                    format!("set<{bits}>")
                }
            },
            TypeDef::Variant(def) => def.name.to_pretty_string(ty_format),
            TypeDef::Function(def) => {
                let mut string = String::new();
                let f = &mut string;
                write!(f, "function (").unwrap();

                for (i, param_ty) in def.param_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ").unwrap();
                    }
                    write!(f, "{}", ty_format(param_ty).as_ref()).unwrap();
                }

                write!(f, "): {}", ty_format(&def.return_ty).as_ref()).unwrap();

                string
            }
        }
    }
}

// sets aren't normal types because they share an underlying type based on their bit width
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SetAliasDef {
    // sets can be anonymous (inline in an `in` expression), in which case we will only ever
    // reference them once and won't need to find them by this name
    pub name: Option<NamePath>,

    pub flags_struct: TypeDefID,
}

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDef::Struct(s) => write!(f, "{}", s),
            TypeDef::Variant(v) => write!(f, "{}", v.name),
            TypeDef::Function(func_ty) => {
                write!(f, "function (")?;

                for (i, param_ty) in func_ty.param_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", param_ty)?;
                }

                write!(f, "): {}", func_ty.return_ty)
            },
        }
    }
}
