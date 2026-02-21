use crate::FieldID;
use crate::TypeDefID;
use crate::Value;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TagInfo {
    pub class_id: TypeDefID,
    pub fields: BTreeMap<FieldID, Value>,
}

impl TagInfo {
    pub fn new(class_id: TypeDefID) -> Self {
        Self {
            class_id,
            fields: BTreeMap::new(),
        }
    }

    pub fn add_field(&mut self, field_id: FieldID, val: impl Into<Value>) {
        self.fields.insert(field_id, val.into());
    }
}