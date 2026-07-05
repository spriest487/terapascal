use crate::StringPath;
use crate::TagInfo;
use crate::Type;
use crate::Value;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VariableInfo {
    pub name: Option<StringPath>,
    pub value_type: Type,

    pub tags: Vec<TagInfo>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConstInfo {
    pub name: StringPath,
    
    pub value: Value,
    pub value_type: Type,

    pub tags: Vec<TagInfo>,
}