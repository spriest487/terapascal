use crate::IRFormatter;
use crate::Type;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::rc::Rc;

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct StringID(pub usize);

impl fmt::Display for StringID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "string literal #{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct MethodID(pub usize);

impl fmt::Display for InterfaceID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct VariableID(pub usize);

impl fmt::Display for VariableID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct TypeDefID(pub usize);

impl TypeDefID {
    pub fn to_class_ptr_type(self) -> Type {
        Type::class_ptr(self)
    }

    pub fn to_class_weak_type(self) -> Type {
        Type::WeakObject(ObjectID::Class(self))
    }

    pub fn to_struct_type(self) -> Type {
        Type::Struct(self)
    }

    pub fn to_variant_type(self) -> Type {
        Type::Variant(self)
    }

    pub fn to_function_type(self) -> Type {
        Type::Function(self)
    }

    pub fn to_pretty_string(self, format: &impl IRFormatter) -> String {
        let mut string = String::new();
        _ = format.format_type_def(self, &mut string);
        
        string
    }
}

impl fmt::Display for TypeDefID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct InterfaceID(pub usize);

impl InterfaceID {
    pub fn to_interface_ptr_type(self) -> Type {
        Type::Object(ObjectID::Interface(self))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum ObjectID {
    // unknown type - may refer to any class type, only known at runtime
    Any,

    //instance of a known class whose layout is defined as the struct with this typedef ID
    Class(TypeDefID),

    // instance of an unknown class that implements the interface with this interface ID
    Interface(InterfaceID),

    // closure of an unknown structure that calls the function type with this typedef ID
    Closure(TypeDefID),

    // array class (dyn array)
    Array(Rc<Type>),

    // boxed value
    Box(Rc<Type>),
}

impl ObjectID {
    pub fn as_class(&self) -> Option<TypeDefID> {
        let ObjectID::Class(class_id) = self else {
            return None;
        };

        Some(*class_id)
    }

    pub fn to_object_type(&self) -> Type {
        Type::Object(self.clone())
    }

    pub fn to_weak_object_type(&self) -> Type {
        Type::WeakObject(self.clone())
    }
}

impl fmt::Display for ObjectID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectID::Any => write!(f, "any"),
            ObjectID::Class(class_id) => write!(f, "{}", class_id),
            ObjectID::Interface(iface_id) => write!(f, "{}", iface_id),
            ObjectID::Closure(closure_id) => write!(f, "{}", closure_id),
            ObjectID::Array(element_type) => write!(f, "array of {}", element_type),
            ObjectID::Box(element_type) => write!(f, "box of {}", element_type),
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

pub const EMPTY_STRING_ID: StringID = StringID(0);

pub const STRING_ID: TypeDefID = TypeDefID(1);
pub const STRING_OBJECT_ID: ObjectID = ObjectID::Class(STRING_ID);
pub const STRING_TYPE: Type = Type::Object(STRING_OBJECT_ID);
pub const STRING_CHARS_FIELD: FieldID = FieldID(0);
pub const STRING_LEN_FIELD: FieldID = FieldID(1);

pub const CLOSURE_PTR_FIELD: FieldID = FieldID(0);

pub const TYPEINFO_ID: TypeDefID = TypeDefID(2);
pub const TYPEINFO_VTYPE_ID: ObjectID = ObjectID::Class(TYPEINFO_ID);
pub const TYPEINFO_TYPE: Type = Type::Object(TYPEINFO_VTYPE_ID);
pub const TYPEINFO_NAME_FIELD: FieldID = FieldID(0);
pub const TYPEINFO_METHODS_FIELD: FieldID = FieldID(1);
pub const TYPEINFO_TAGS_FIELD: FieldID = FieldID(2);
pub const TYPEINFO_IMPL_FIELD: FieldID = FieldID(3);
pub const TYPEINFO_FLAGS_FIELD: FieldID = FieldID(4);
pub const TYPEINFO_FLAGS_BITS: usize = 64;

pub const METHODINFO_ID: TypeDefID = TypeDefID(3);
pub const METHODINFO_VTYPE_ID: ObjectID = ObjectID::Class(METHODINFO_ID);
pub const METHODINFO_TYPE: Type = Type::Object(METHODINFO_VTYPE_ID);
pub const METHODINFO_NAME_FIELD: FieldID = FieldID(0);
pub const METHODINFO_OWNER_FIELD: FieldID = FieldID(1);
pub const METHODINFO_IMPL_FIELD: FieldID = FieldID(2);
pub const METHODINFO_TAGS_FIELD: FieldID = FieldID(3);

pub const FUNCINFO_ID: TypeDefID = TypeDefID(4);
pub const FUNCINFO_VTYPE_ID: ObjectID = ObjectID::Class(FUNCINFO_ID);
pub const FUNCINFO_TYPE: Type = Type::Object(FUNCINFO_VTYPE_ID);
pub const FUNCINFO_NAME_FIELD: FieldID = FieldID(0);
pub const FUNCINFO_IMPL_FIELD: FieldID = FieldID(1);
pub const FUNCINFO_TAGS_FIELD: FieldID = FieldID(2);

pub const ANY_TYPE: Type = Type::Object(ObjectID::Any);

pub const RESERVED_TYPES: [TypeDefID; 4] = [
    STRING_ID,
    TYPEINFO_ID,
    METHODINFO_ID,
    FUNCINFO_ID,
];

pub const RESERVED_STRINGS: [StringID; 1] = [
    EMPTY_STRING_ID,
];
