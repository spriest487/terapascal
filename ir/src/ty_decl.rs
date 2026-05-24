mod r#struct;
mod variant;
mod interface;
mod tags;

use crate::FunctionID;
use crate::FunctionRef;
use crate::FunctionSig;
use crate::IRFormatter;
use crate::NamePath;
use crate::ObjectID;
use crate::RawFormatter;
use crate::Ref;
use crate::TagInfo;
use crate::Type;
pub use interface::*;
pub use r#struct::*;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::rc::Rc;
pub use tags::*;
pub use variant::*;

pub use crate::metadata::ids::InterfaceID;
pub use crate::metadata::ids::TypeDefID;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum StructIdentity {
    Record(NamePath),
    Class(NamePath),
    ClosureObject(ClosureIdentity),
    
    // bitmask type for set flag collections. value is the number of bits
    SetFlags { bits: usize }
}

impl StructIdentity {
    pub fn to_pretty_string(&self, formatter: &impl IRFormatter) -> String {
        match self {
            StructIdentity::Record(name) | StructIdentity::Class(name) => {
                name.to_pretty_string(formatter)
            },
            StructIdentity::ClosureObject(id) => {
                let func_ref = Ref::from(FunctionRef::new(id.id));
                let func_ty = Type::Function(id.sig.clone());

                format!(
                    "closure object ({}: {})",
                    func_ref.to_pretty_string(formatter),
                    func_ty.to_pretty_string(formatter),
                )
            }
            StructIdentity::SetFlags { bits } => {
                format!("set{bits}")
            },
        }
    }

    pub fn to_definition_type(&self, id: TypeDefID) -> Type {
        match self {
            StructIdentity::Record(name) => id.to_struct_type(name.type_args.clone()),

            StructIdentity::Class(name) => id.to_class_ptr_type(name.type_args.clone()),

            // the type of a closure definition is not a (virtual) closure pointer object, it's
            // an anonymous class
            StructIdentity::ClosureObject(..) => id.to_class_ptr_type([]),

            StructIdentity::SetFlags { .. } => id.to_flags_type(),
        }
    }

    pub fn name(&self) -> Option<&NamePath> {
        match self {
            StructIdentity::Class(name)
            | StructIdentity::Record(name) => Some(name),

            StructIdentity::ClosureObject(..)
            | StructIdentity::SetFlags { .. } => None,
        }
    }

    pub fn name_mut(&mut self) -> Option<&mut NamePath> {
        match self {
            StructIdentity::Class(name)
            | StructIdentity::Record(name) => Some(name),

            StructIdentity::ClosureObject(..)
            | StructIdentity::SetFlags { .. } => None,
        }
    }
}

impl fmt::Display for StructIdentity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
       write!(f, "{}", self.to_pretty_string(&RawFormatter))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ClosureIdentity {
    // the virtual sig of the closure type, which doesn't include the initial closure pointer
    // parameter the actual function/function pointer type has
    pub sig: Rc<FunctionSig>,

    // the ID of the real function that implements this closure instance
    pub id: FunctionID,
}

impl ClosureIdentity {
    pub fn to_closure_ptr_type(&self) -> Type {
        Type::Object(ObjectID::AnyClosure(self.sig.clone()))
    }
}

impl StructIdentity {
    pub fn is_ref_type(&self) -> bool {
        match self {
            | StructIdentity::Record(..)
            | StructIdentity::SetFlags { .. } => false,

            StructIdentity::Class(..)
            | StructIdentity::ClosureObject(..) => true,
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
    Struct(StructDef),
    Variant(VariantDef),
    Function(Rc<FunctionSig>),
}

impl TypeDef {
    pub fn name(&self) -> Option<&NamePath> {
        match self {
            TypeDef::Struct(s) => s.name(),
            TypeDef::Variant(v) => Some(&v.name),
            TypeDef::Function(..) => None,
        }
    }
    
    pub fn tags(&self) -> &[TagInfo] {
        match self {
            TypeDef::Struct(def) => &def.tags,
            TypeDef::Variant(def) => &def.tags,
            TypeDef::Function(..) => &[],
        }
    }
    
    pub fn to_pretty_string(&self, formatter: &impl IRFormatter) -> String {
        match self {
            TypeDef::Struct(def) => {
                def.identity.to_pretty_string(formatter)
            },
            
            TypeDef::Variant(def) => {
                def.name.to_pretty_string(formatter)
            },
            
            TypeDef::Function(def) => {
                let mut string = String::new();
                string.push_str("function (");

                for (i, param_ty) in def.param_types.iter().enumerate() {
                    if i > 0 {
                        string.push_str("; ");
                    }

                    string.push_str(&param_ty.to_pretty_string(formatter));
                }

                string.push_str("): ");
                string.push_str(&def.result_type.to_pretty_string(formatter));

                string
            },
        }
    }
    
    pub fn is_class(&self) -> bool {
        match self {
            TypeDef::Struct(struct_def) => struct_def.is_class(),
            _ => false,
        }
    }
}

// sets aren't normal types because they share an underlying type based on their bit width
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SetAliasDef {
    pub name: NamePath,

    pub flags_struct: TypeDefID,
}

impl fmt::Display for TypeDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeDef::Struct(s) => write!(f, "{}", s),
            TypeDef::Variant(v) => write!(f, "{}", v.name),
            TypeDef::Function(func_ty) => {
                write!(f, "function (")?;

                for (i, param_ty) in func_ty.param_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", param_ty)?;
                }

                write!(f, "): {}", func_ty.result_type)
            },
        }
    }
}
