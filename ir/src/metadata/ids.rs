use crate::{FunctionSig, IRFormatter};
use crate::Type;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct StringID(pub usize);

impl fmt::Display for StringID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "string literal {}", self.0)
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
        write!(f, "%Var{}", self.0)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug, Ord, PartialOrd, Serialize, Deserialize)]
pub struct TypeDefID(pub usize);

impl TypeDefID {
    pub fn to_class_ptr_type(self, args: impl IntoIterator<Item=Type>) -> Type {
        Type::class_ptr(self, args)
    }

    pub fn to_class_weak_type(self, args: impl IntoIterator<Item=Type>) -> Type {
        Type::WeakObject(ObjectID::Class(TypeRef::new(self, args)))
    }

    pub fn to_struct_type(self, args: impl IntoIterator<Item=Type>) -> Type {
        Type::Struct(TypeRef::new(self, args))
    }

    pub fn to_variant_type(self, args: impl IntoIterator<Item=Type>) -> Type {
        Type::Variant(TypeRef::new(self, args))
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

#[derive(Eq, PartialEq, Hash, Clone, Debug, Serialize, Deserialize)]
pub struct TypeRef {
    pub def_id: TypeDefID,
    pub args: Vec<Type>,
}

impl TypeRef {
    pub fn new(def_id: TypeDefID, args: impl IntoIterator<Item=Type>) -> Rc<Self> {
        Rc::new(Self {
            def_id,
            args: args.into_iter().collect(),
        })
    }

    pub fn to_struct_type(self: &Rc<Self>) -> Type {
        Type::Struct(self.clone())
    }

    pub fn to_variant_type(self: &Rc<Self>) -> Type {
        Type::Variant(self.clone())
    }

    pub fn to_class_object_type(self: &Rc<Self>) -> Type {
        Type::Object(ObjectID::Class(self.clone()))
    }

    pub fn to_weak_class_object_type(self: &Rc<Self>) -> Type {
        Type::WeakObject(ObjectID::Class(self.clone()))
    }

    pub fn is_generic(&self) -> bool {
        if self.args.is_empty() {
            return false;
        }

        self.args.iter().all(|ty| {
            ty.as_generic_param().is_some()
        })
    }
}

impl fmt::Display for TypeRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.def_id)?;

        if !self.args.is_empty() {
            write!(f, "[")?;
            for (i, arg) in self.args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{arg}")?;
            }
            write!(f, "]")?;
        }

        Ok(())
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
    Class(Rc<TypeRef>),

    // instance of an unknown class that implements the interface with this interface ID
    Interface(InterfaceID),

    // closure of an unknown structure that calls the function type with this virtual sig
    AnyClosure(Rc<FunctionSig>),

    // array class (dyn array)
    Array(Rc<Type>),

    // boxed value
    Box(Rc<Type>),
}

impl ObjectID {
    pub fn as_class(&self) -> Option<&Rc<TypeRef>> {
        let ObjectID::Class(type_id) = self else {
            return None;
        };

        Some(type_id)
    }

    pub fn to_object_type(&self) -> Type {
        Type::Object(self.clone())
    }

    pub fn to_weak_object_type(&self) -> Type {
        Type::WeakObject(self.clone())
    }

    pub fn is_abstract(&self) -> bool {
        match self {
            ObjectID::Any
            | ObjectID::AnyClosure(..)
            | ObjectID::Interface(..) => true,

            ObjectID::Class(..)
            | ObjectID::Box(..)
            | ObjectID::Array(..) => false,
        }
    }
}

impl fmt::Display for ObjectID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectID::Any => write!(f, "any"),
            ObjectID::Class(class_id) => write!(f, "{}", class_id),
            ObjectID::Interface(iface_id) => write!(f, "{}", iface_id),
            ObjectID::AnyClosure(closure_id) => write!(f, "{}", closure_id),
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
pub const STRING_CHARS_FIELD: FieldID = FieldID(0);
pub const STRING_LEN_FIELD: FieldID = FieldID(1);

pub const CLOSURE_PTR_FIELD: FieldID = FieldID(0);

pub const TYPEINFO_ID: TypeDefID = TypeDefID(2);
pub const TYPEINFO_NAME_FIELD: FieldID = FieldID(0);
pub const TYPEINFO_METHODS_FIELD: FieldID = FieldID(1);
pub const TYPEINFO_TAGS_FIELD: FieldID = FieldID(2);
pub const TYPEINFO_IMPL_FIELD: FieldID = FieldID(3);
pub const TYPEINFO_FLAGS_FIELD: FieldID = FieldID(4);
pub const TYPEINFO_FLAGS_BITS: usize = 64;

pub const METHODINFO_ID: TypeDefID = TypeDefID(3);
pub const METHODINFO_NAME_FIELD: FieldID = FieldID(0);
pub const METHODINFO_OWNER_FIELD: FieldID = FieldID(1);
pub const METHODINFO_IMPL_FIELD: FieldID = FieldID(2);
pub const METHODINFO_TAGS_FIELD: FieldID = FieldID(3);

pub const FUNCINFO_ID: TypeDefID = TypeDefID(4);
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

thread_local!{
    static STRING_OBJECT_ID: Rc<TypeRef> = TypeRef::new(STRING_ID, []);
    static TYPEINFO_OBJECT_ID: Rc<TypeRef> = TypeRef::new(TYPEINFO_ID, []);
    static FUNCINFO_OBJECT_ID: Rc<TypeRef> = TypeRef::new(FUNCINFO_ID, []);
    static METHODINFO_OBJECT_ID: Rc<TypeRef> = TypeRef::new(METHODINFO_ID, []);

    static STRING_TYPE: Type = STRING_OBJECT_ID
        .try_with(|id| id.to_class_object_type())
        .unwrap();

    static TYPEINFO_TYPE: Type = TYPEINFO_OBJECT_ID
        .try_with(|id| id.to_class_object_type())
        .unwrap();

    static FUNCINFO_TYPE: Type = FUNCINFO_OBJECT_ID
        .try_with(|id| id.to_class_object_type())
        .unwrap();

    static METHODINFO_TYPE: Type = METHODINFO_OBJECT_ID
        .try_with(|id| id.to_class_object_type())
        .unwrap();
}

impl Type {
    pub fn string() -> Type {
        STRING_TYPE.try_with(|t| t.clone()).unwrap()
    }

    pub fn type_info() -> Type {
        TYPEINFO_TYPE.try_with(|t| t.clone()).unwrap()
    }

    pub fn func_info() -> Type {
        FUNCINFO_TYPE.try_with(|t| t.clone()).unwrap()
    }

    pub fn method_info() -> Type {
        METHODINFO_TYPE.try_with(|t| t.clone()).unwrap()
    }
}