use crate::NamePath;
use crate::Type;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct VariableInfo {
    pub name: NamePath,
    pub r#type: Type,
}
