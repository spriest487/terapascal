use crate::DeclPath;
use crate::TagInfo;
use crate::Type;
use crate::Value;
use crate::Visibility;
use serde::Deserialize;
use serde::Serialize;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VariantCase {
    pub name: String,
    pub tag: Value,
    pub ty: Option<Type>,
}

impl VariantCase {
    pub fn new(name: impl Into<String>, tag: Value) -> Self {
        Self {
            name: name.into(),
            tag,
            ty: None,
        }
    }
    
    pub fn with_data(mut self, ty: Type) -> Self {
        self.ty = Some(ty);
        self
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VariantDef {
    pub name: DeclPath,
    pub tag_type: Type,

    pub visibility: Visibility,

    pub cases: Vec<VariantCase>,
    
    pub tags: Vec<TagInfo>,
}

impl VariantDef {
    pub fn is_generic(&self) -> bool {
        !self.name.type_params.is_empty()
    }
}
