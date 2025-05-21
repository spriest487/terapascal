use crate::FieldID;
use crate::InterfaceID;
use crate::TypeDefID;
use crate::Value;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Tag {
    pub tag_class: TypeDefID,
    pub tag_args: Vec<TagArg>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TagArg {
    pub field_id: FieldID,
    pub value: Value,
}



#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum TagLocation {
    TypeDef(TypeDefID),
    Interface(InterfaceID),
    Method { type_id: TypeDefID, method_index: usize },
    InterfaceMethod { iface_id: InterfaceID, method_index: usize },
}

impl TagLocation {    
    pub fn method_loc(self, method_index: usize) -> Option<Self> {
        match self {
            TagLocation::TypeDef(id) => {
                Some(TagLocation::Method { method_index, type_id: id })
            },

            TagLocation::Interface(id) => {
                Some(TagLocation::InterfaceMethod { method_index, iface_id: id })
            },

            _ => None,
        }
    }
}

impl fmt::Display for TagLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "tags(")?;
        match self {
            TagLocation::TypeDef(id) => write!(f, "type {}", id.0)?,
            TagLocation::Interface(id) => write!(f, "interface {}", id.0)?,
            TagLocation::Method { type_id: self_ty, method_index} => write!(f, "type {}/method {}", self_ty.0, method_index)?,
            TagLocation::InterfaceMethod { iface_id, method_index} => write!(f, "iface {}/method {}", iface_id.0, method_index)?,
        }
        write!(f, ")")
    }
}
