pub use crate::metadata::ids::FieldID;
pub use crate::metadata::ids::ObjectID;
use crate::metadata::STRING_ID;
use crate::ty_decl::InterfaceID;
use crate::ty_decl::TypeDefID;
use crate::IRFormatter;
use crate::MetadataSource;
use crate::TagLocation;
use crate::Value;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum Type {
    /// no type (used for raw pointers like void*)
    Nothing,

    /// Named generic placeholder
    Generic(Rc<String>),

    Pointer(Rc<Type>),
    TempRef(Rc<Type>),

    Struct(TypeDefID),
    Variant(TypeDefID),
    Flags(TypeDefID),
    Array {
        element: Rc<Type>,
        dim: usize,
    },

    /// pointer to an RC object somewhere on the heap, which can be dereferenced to yield a value
    /// of the inner type. the resource type is Some in the case that the type is known, and
    /// None for the Any type
    Object(ObjectID),
    WeakObject(ObjectID),

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
    F64,
}

impl Type {
    pub fn ptr(&self) -> Self {
        Type::Pointer(Rc::new(self.clone()))
    }
    
    pub fn temp_ref(&self) -> Self {
        Type::TempRef(Rc::new(self.clone()))
    }

    pub const fn any() -> Self {
        Type::Object(ObjectID::Any)
    }

    pub const fn class_ptr(id: TypeDefID) -> Self {
        Self::object_ptr(ObjectID::Class(id))
    }
    
    pub const fn object_ptr(class: ObjectID) -> Self {
        Type::Object(class)
    }
    
    pub const fn weak_object_ptr(class: ObjectID) -> Self {
        Type::WeakObject(class)
    }

    pub const fn string_ptr() -> Self {
        Self::class_ptr(STRING_ID)
    }

    pub fn deref_ty(&self) -> Option<&Self> {
        match self {
            Type::Pointer(target) => Some(target),
            Type::TempRef(target) => Some(target),
            _ => None,
        }
    }

    pub fn array(&self, dim: usize) -> Self {
        Type::Array {
            dim,
            element: Rc::new(self.clone()),
        }
    }

    pub fn dyn_array(&self) -> Self {
        Type::Object(ObjectID::Array(Rc::new(self.clone())))
    }

    pub fn weak_dyn_array(self) -> Self {
        Type::WeakObject(ObjectID::Array(Rc::new(self)))
    }

    pub fn dyn_array_element(&self) -> Option<&Self> {
        match self {
            Type::Object(ObjectID::Array(element_type)) => Some(element_type.as_ref()),
            _ => None,
        }
    }

    pub fn boxed(self) -> Self {
        Type::Object(ObjectID::Box(Rc::new(self)))
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
    
    pub fn as_variant(&self) -> Option<TypeDefID> {
        match self {
            Type::Variant(id) => Some(*id),
            _ => None,
        }
    }
    
    pub fn iface_ptr(id: InterfaceID) -> Self {
        Type::Object(ObjectID::Interface(id))
    }

    pub fn as_iface(&self) -> Option<InterfaceID> {
        match self {
            Type::Object(ObjectID::Interface(id)) => Some(*id),
            Type::WeakObject(ObjectID::Interface(id)) => Some(*id),
            _ => None,
        }
    }

    pub fn is_object(&self) -> bool {
        matches!(self, Type::Object(..) | Type::WeakObject(..))
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(..))
    }

    pub fn is_temp_ref(&self) -> bool {
        matches!(self, Type::TempRef(..))
    }

    pub fn is_complex(&self) -> bool {
        matches!(self, Type::Variant(..) | Type::Array { .. } | Type::Struct(..))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::U8 | Type::I8
            | Type::I16 | Type::U16
            | Type::I32 | Type::U32
            | Type::I64 | Type::U64
            | Type::USize | Type::ISize
            => true,

            _ => false,
        }
    }

    pub fn rc_resource_class_id(&self) -> Option<&ObjectID> {
        match self {
            Type::Object(class_id) => Some(class_id),
            Type::WeakObject(class_id) => Some(class_id),
            _ => None,
        }
    }
    
    pub fn is_weak(&self) -> bool {
        matches!(self, Type::WeakObject(..))
    }

    // when a value of this type is the target of a field reference, the id of the struct
    // definition that contains the field that can be accessed through this type, or None if the
    // type does not have fields
    pub fn field_struct_id(&self) -> Option<TypeDefID> {
        match self {
            Type::Struct(id) => Some(*id),
            Type::Object(ObjectID::Class(id)) => Some(*id),
            _ => None,
        }
    }

    // when a value of this type is the target of an element reference, the type of element ref
    // that will be created
    pub fn element_type(&self) -> Option<&Self> {
        match self {
            Type::Array { element, .. } => Some(element.as_ref()),
            Type::Object(ObjectID::Box(value_type)) => Some(value_type.as_ref()),
            Type::Object(ObjectID::Array(element_type)) => Some(element_type.as_ref()),
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
            ObjectID::Class(id) => Some(*id),
            _ => None,
        }
    }
    
    pub fn tags_loc(&self) -> Option<TagLocation> {
        match self {
            | Type::Object(ObjectID::Class(id))
            | Type::Struct(id)
            | Type::Flags(id)
            | Type::Variant(id) => Some(TagLocation::TypeDef(*id)),

            | Type::Object(ObjectID::Interface(id)) => Some(TagLocation::Interface(*id)),

            | _ => None,
        }
    }
    
    pub fn to_pretty_string(&self, formatter: &(impl IRFormatter + ?Sized)) -> String {
        let mut result = String::new();
        _ = formatter.format_type(self, &mut result);
        result
    }
    
    pub fn default_literal(&self) -> Option<Value> {
        match self {
            Type::Pointer(_)
            | Type::Object(_)
            | Type::WeakObject(_)
            | Type::Function(_) => {
                Some(Value::LiteralNil)
            },

            Type::Bool => Some(Value::LiteralBool(false)),
            Type::U8 => Some(Value::LiteralU8(0)),
            Type::I8 => Some(Value::LiteralI8(0)),
            Type::I16 => Some(Value::LiteralI16(0)),
            Type::U16 => Some(Value::LiteralU16(0)),
            Type::I32 => Some(Value::LiteralI32(0)),
            Type::U32 => Some(Value::LiteralU32(0)),
            Type::I64 => Some(Value::LiteralI64(0)),
            Type::U64 => Some(Value::LiteralU64(0)),
            Type::USize => Some(Value::LiteralUSize(0)),
            Type::ISize => Some(Value::LiteralISize(0)),
            Type::F32 => Some(Value::LiteralF32(0.0)),
            Type::F64 => Some(Value::LiteralF64(0.0)),

            Type::Generic(..)
            | Type::Struct(..)
            | Type::TempRef(..)
            | Type::Flags(..)
            | Type::Variant(..)
            | Type::Array { .. }
            | Type::Nothing => {
                None
            }
        }
    }

    pub fn default_value(&self) -> Value {
        Value::Default(self.clone())
    }
    
    pub fn contains_any_object_refs(&self, metadata: &impl MetadataSource) -> bool {
        match self {
            Type::Object(_) | Type::WeakObject(_) => true,

            // generics may contain object pointers, and we should emit retain/release instructions
            // for them, to be replaced as appropriate later
            Type::Generic(..) => true,

            Type::Struct(id) => {
                let Some(def) = metadata.get_struct_def(*id) else {
                    return false;
                };

                def.fields
                    .values()
                    .any(|f| f.ty.contains_any_object_refs(metadata))
            }
            Type::Variant(id) => {
                let Some(def) = metadata.get_variant_def(*id) else {
                    return false;
                };

                def.cases
                    .iter()
                    .filter_map(|case| case.ty.as_ref())
                    .any(|ty| ty.contains_any_object_refs(metadata))
            }
            
            Type::Array { element, dim } => {
                *dim > 0 && element.contains_any_object_refs(metadata)
            }

            Type::Nothing
            | Type::Pointer(_)
            | Type::TempRef(_)
            | Type::Flags(_)
            | Type::Function(_)
            | Type::Bool
            | Type::U8
            | Type::I8
            | Type::I16
            | Type::U16
            | Type::I32
            | Type::U32
            | Type::I64
            | Type::U64
            | Type::USize
            | Type::ISize
            | Type::F32
            | Type::F64 => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Nothing => write!(f, "none"),
            Type::Generic(name) => write!(f, "{}", name),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
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
            Type::TempRef(target) => write!(f, "&{}", target),
            Type::Struct(id) => write!(f, "{{struct {}}}", id),
            Type::Variant(id) => write!(f, "{{variant {}}}", id),
            Type::Flags(repr_id) => write!(f, "{{flags {}}}", repr_id),
            Type::Object(id) => match id {
                ObjectID::Any => write!(f, "any"),
                ObjectID::Class(id) => write!(f, "class {}", id),
                ObjectID::Interface(id) => write!(f, "iface {}", id),
                ObjectID::Closure(id) => write!(f, "closure {}", id),
                ObjectID::Array(element) => write!(f, "array of {}", element),
                ObjectID::Box(element) => write!(f, "box of {}", element),
            },
            Type::WeakObject(id) => match id {
                ObjectID::Any => write!(f, "weak any"),
                ObjectID::Class(id) => write!(f, "weak class {}", id),
                ObjectID::Interface(id) => write!(f, "weak iface {}", id),
                ObjectID::Closure(id) => write!(f, "weak closure {}", id),
                ObjectID::Array(element) => write!(f, "weak array of {}", element),
                ObjectID::Box(element) => write!(f, "weak box of {}", element),
            },
            Type::Array { element, dim } => write!(f, "array[{}] of {}", dim, element),
            Type::Function(id) => write!(f, "function {}", id),
        }
    }
}
