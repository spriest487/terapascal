use crate::NamePath;
use crate::Type;
use crate::Value;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableInfo {
    pub name: NamePath,
    pub r#type: Type,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstInfo {
    pub name: NamePath,
    pub value: Value,
}