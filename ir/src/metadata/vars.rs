use crate::NamePath;
use crate::TagInfo;
use crate::Type;
use crate::Value;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableInfo {
    pub name: Option<NamePath>,
    pub value_type: Type,

    pub tags: Vec<TagInfo>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstInfo {
    pub name: NamePath,
    
    pub value: Value,
    pub value_type: Type,

    pub tags: Vec<TagInfo>,
}