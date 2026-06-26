use crate::{FunctionID, StructIdentity};
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

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TypeInfo {
    pub name: StringID,
    pub debug_name: Option<String>,

    pub flags: u64,
    
    pub methods: Vec<MethodInfo>,
}

impl TypeInfo {
    pub fn new(name: StringID, flags: u64) -> Self {
        Self {
            name,
            debug_name: None,
            
            flags,
            
            methods: Vec::new(),
        }
    }

    pub fn get_name_string<'m>(&self, metadata: &'m Metadata) -> Option<&'m String> {
        metadata.get_string(self.name)
    }

    pub fn type_runtime_flags(ty: &Type, metadata: &impl MetadataSource) -> u64 {
        let mut flags = 0;

        if !ty.is_object() {
            flags |= TYPE_FLAG_VALUE;
        }

        if matches!(ty, Type::Array {..} | Type::Object(ObjectID::Array(..))) {
            flags |= TYPE_FLAG_ARRAY;
        }
        if let Type::WeakObject(class_id) = ty {
            // weak pointers should have the same flags as their non-weak version + the weak flag
            flags |= Self::type_runtime_flags(&Type::Object(class_id.clone()), metadata);
            flags |= TYPE_FLAG_WEAK;
        }
        if matches!(ty, Type::Function(..) | Type::Object(ObjectID::AnyClosure(..))) {
            flags |= TYPE_FLAG_FUNCTION;
        }

        if let Type::Object(ObjectID::Class(class_ref)) = ty
            && let Some(class_def) = metadata.get_struct_def(class_ref.def_id)
            && matches!(class_def.identity, StructIdentity::ClosureObject(..))
        {
            flags |= TYPE_FLAG_FUNCTION;
        }

        flags
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
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
