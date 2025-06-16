use crate::metadata::STRING_ID;
use crate::ty_decl::InterfaceID;
use crate::ty_decl::SetAliasID;
use crate::ty_decl::TypeDefID;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::rc::Rc;
use crate::TagLocation;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum Type {
    /// no type (used for raw pointers like void*)
    Nothing,

    Pointer(Rc<Type>),
    Struct(TypeDefID),
    Variant(TypeDefID),
    Flags(TypeDefID, SetAliasID),
    Array {
        element: Rc<Type>,
        dim: usize,
    },

    /// pointer to an RC object somewhere on the heap, which can be dereferenced to yield a value
    /// of the inner type. the resource type is Some in the case that the type is known, and
    /// None for the Any type
    RcPointer(VirtualTypeID),
    RcWeakPointer(VirtualTypeID),

    // Function pointer type for a function
    Function(TypeDefID),

    Bool,
    U8,
    I8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    USize,
    ISize,
    F32,
}

impl Type {
    pub fn ptr(self) -> Self {
        Type::Pointer(Rc::new(self))
    }

    pub fn any() -> Self {
        Type::RcPointer(VirtualTypeID::Any)
    }

    pub const fn class_ptr(id: TypeDefID) -> Self {
        Self::rc_ptr_to(VirtualTypeID::Class(id))
    }
    
    pub const fn rc_ptr_to(class: VirtualTypeID) -> Self {
        Type::RcPointer(class)
    }
    
    pub const fn rc_weak_ptr_to(class: VirtualTypeID) -> Self {
        Type::RcWeakPointer(class)
    }

    pub const fn rc_ptr_any() -> Self {
        Type::RcPointer(VirtualTypeID::Any)
    }

    pub const fn string_ptr() -> Self {
        Type::rc_ptr_to(VirtualTypeID::Class(STRING_ID))
    }

    pub fn deref_ty(&self) -> Option<&Self> {
        match self {
            Type::Pointer(target) => Some(target),
            _ => None,
        }
    }

    pub fn array(self, dim: usize) -> Self {
        Type::Array {
            dim,
            element: Rc::new(self),
        }
    }

    pub fn as_struct(&self) -> Option<TypeDefID> {
        match self {
            Type::Struct(struct_id) => Some(*struct_id),
            _ => None,
        }
    }

    pub fn is_struct(&self, id: TypeDefID) -> bool {
        match self {
            Type::Struct(ty_id) => *ty_id == id,
            _ => false,
        }
    }
    
    pub fn iface_ptr(id: InterfaceID) -> Self {
        Type::RcPointer(VirtualTypeID::Interface(id))
    }

    pub fn as_iface(&self) -> Option<InterfaceID> {
        match self {
            Type::RcPointer(VirtualTypeID::Interface(id)) => Some(*id),
            Type::RcWeakPointer(VirtualTypeID::Interface(id)) => Some(*id),
            _ => None,
        }
    }

    pub fn is_rc(&self) -> bool {
        matches!(self, Type::RcPointer(..) | Type::RcWeakPointer(..))
    }

    pub fn is_complex(&self) -> bool {
        matches!(self, Type::Variant(..) | Type::Array { .. } | Type::Struct(..))
    }

    pub fn rc_resource_class_id(&self) -> Option<VirtualTypeID> {
        match self {
            Type::RcPointer(class_id) => Some(*class_id),
            Type::RcWeakPointer(class_id) => Some(*class_id),
            _ => None,
        }
    }
    
    pub fn def_id(&self) -> Option<TypeDefID> {
        match self {
            Type::Variant(id)
            | Type::Function(id)
            | Type::Flags(id, ..)
            | Type::Struct(id) => {
                Some(*id)
            }

            _ => None,
        }
    }
    
    pub fn rc_resource_def_id(&self) -> Option<TypeDefID> {
        match self.rc_resource_class_id()? {
            VirtualTypeID::Class(id)
            | VirtualTypeID::Closure(id) => Some(id),
            _ => None,
        }
    }
    
    pub fn tags_loc(&self) -> Option<TagLocation> {
        match self {
            | Type::RcPointer(VirtualTypeID::Class(id))
            | Type::Struct(id)
            | Type::Flags(id, _)
            | Type::Variant(id) => Some(TagLocation::TypeDef(*id)),

            | Type::RcPointer(VirtualTypeID::Interface(id)) => Some(TagLocation::Interface(*id)),

            | _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nothing => write!(f, "none"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::U8 => write!(f, "u8"),
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::U16 => write!(f, "u16"),
            Type::I32 => write!(f, "i32"),
            Type::U32 => write!(f, "u32"),
            Type::I64 => write!(f, "i64"),
            Type::U64 => write!(f, "u64"),
            Type::ISize => write!(f, "isize"),
            Type::USize => write!(f, "usize"),
            Type::Pointer(target) => write!(f, "^{}", target),
            Type::Struct(id) => write!(f, "{{struct {}}}", id),
            Type::Variant(id) => write!(f, "{{variant {}}}", id),
            Type::Flags(_repr_id, set_id) => write!(f, "{{flags {}}}", set_id),
            Type::RcPointer(id) => match id {
                VirtualTypeID::Any => write!(f, "any"),
                VirtualTypeID::Class(id) => write!(f, "class {}", id),
                VirtualTypeID::Interface(id) => write!(f, "iface {}", id),
                VirtualTypeID::Closure(id) => write!(f, "closure {}", id),
            },
            Type::RcWeakPointer(id) => match id {
                VirtualTypeID::Any => write!(f, "weak any"),
                VirtualTypeID::Class(id) => write!(f, "weak class {}", id),
                VirtualTypeID::Interface(id) => write!(f, "weak iface {}", id),
                VirtualTypeID::Closure(id) => write!(f, "weak closure {}", id),
            },
            Type::Array { element, dim } => write!(f, "{}[{}]", element, dim),
            Type::Function(id) => write!(f, "function {}", id),
        }
    }
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum VirtualTypeID {
    // unknown type - may refer to any class type, only known at runtime
    Any,

    //instance of a known class whose layout is defined as the struct with this typedef ID
    Class(TypeDefID),

    // instance of an unknown class that implements the interface with this interface ID
    Interface(InterfaceID),

    // closure of an unknown structure that calls the function type with this typedef ID
    Closure(TypeDefID),
}

impl VirtualTypeID {
    pub fn as_class(&self) -> Option<TypeDefID> {
        match self {
            VirtualTypeID::Class(id) => Some(*id),
            VirtualTypeID::Any | VirtualTypeID::Interface(..) | VirtualTypeID::Closure(..) => None,
        }
    }
}

impl fmt::Display for VirtualTypeID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VirtualTypeID::Any => write!(f, "any"),
            VirtualTypeID::Class(struct_id) => write!(f, "{}", struct_id),
            VirtualTypeID::Interface(iface_id) => write!(f, "{}", iface_id),
            VirtualTypeID::Closure(closure_id) => write!(f, "{}", closure_id),
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct FieldID(pub usize);

impl fmt::Display for FieldID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

