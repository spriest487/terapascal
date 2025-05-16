use std::fmt;
use crate::{FieldID, InterfaceID};
use crate::TypeDefID;
use crate::Value;
use serde::Deserialize;
use serde::Serialize;

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
}

impl fmt::Display for TagLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "tags(")?;
        match self {
            TagLocation::TypeDef(id) => write!(f, "type {}", id.0)?,
            TagLocation::Interface(id) => write!(f, "interface {}", id.0)?,
        }
        write!(f, ")")
    }
}
