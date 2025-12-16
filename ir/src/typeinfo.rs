use crate::FunctionID;
use crate::Metadata;
use crate::MetadataSource;
use crate::ObjectID;
use crate::StringID;
use crate::TagInfo;
use crate::Type;
use serde::Deserialize;
use serde::Serialize;

pub const TYPE_FLAG_VALUE: u64 = 1 << 0;
pub const TYPE_FLAG_WEAK: u64 = 1 << 1;
pub const TYPE_FLAG_ARRAY: u64 = 1 << 2;
pub const TYPE_FLAG_FUNCTION: u64 = 1 << 3;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TypeInfo {
    pub name: Option<StringID>,
    pub debug_name: Option<String>,

    pub flags: u64,
    
    pub dtor: Option<FunctionID>,
    
    pub methods: Vec<MethodInfo>,
}

impl TypeInfo {
    pub fn new(name: Option<StringID>, flags: u64) -> Self {
        Self {
            name,
            debug_name: None,
            
            flags,
            
            methods: Vec::new(),
            dtor: None,
        }
    }

    pub fn get_name_string<'m>(&self, metadata: &'m Metadata) -> Option<&'m String> {
        let id = self.name?;
        metadata.get_string(id)
    }

    pub fn type_runtime_flags(ty: &Type) -> u64 {
        let mut flags = 0;
        if !ty.is_object() {
            flags |= TYPE_FLAG_VALUE;
        }
        if matches!(ty, Type::Array {..} | Type::Object(ObjectID::Array(..))) {
            flags |= TYPE_FLAG_ARRAY;
        }
        if let Type::WeakObject(class_id) = ty {
            // weak pointers should have the same flags as their non-weak version + the weak flag
            flags |= Self::type_runtime_flags(&Type::Object(class_id.clone()));
            flags |= TYPE_FLAG_WEAK;
        }
        if matches!(ty, Type::Function(..) | Type::Object(ObjectID::Closure(..))) {
            flags |= TYPE_FLAG_FUNCTION;
        }

        flags
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MethodInfo {
    pub name: StringID,
    pub index: usize,
    
    pub instance_ty: Type,

    // None for abstract methods
    pub function: Option<FunctionID>,

    pub result_ty: Type,
    pub params: Vec<Type>,

    pub tags: Vec<TagInfo>,
}
