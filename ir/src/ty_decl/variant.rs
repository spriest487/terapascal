use crate::Type;
use crate::NamePath;
use serde::Deserialize;
use serde::Serialize;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VariantCase {
    pub name: String,
    pub ty: Option<Type>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VariantDef {
    pub name: NamePath,
    pub tag_type: Type,

    pub cases: Vec<VariantCase>,
}
