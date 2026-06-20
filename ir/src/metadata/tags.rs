use crate::FieldID;
use crate::IRFormatter;
use crate::TypeDefID;
use crate::Value;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::fmt;

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

    pub fn format_pretty(
        &self,
        formatter: &impl IRFormatter,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        let class_type = self.class_id.to_struct_type([]);

        write!(f, "[")?;
        formatter.format_type(&class_type, f)?;

        if !self.fields.is_empty() {
            write!(f, "(")?;

            for (i, (field_id, value)) in self.fields.iter().enumerate() {
                if i > 0 {
                    write!(f, "; ")?;
                }

                formatter.format_field(&class_type, *field_id, f)?;
                write!(f, " = ")?;
                formatter.format_val(value, f)?;
            }

            write!(f, ")")?;
        }

        write!(f, "]")
    }
}