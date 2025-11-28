use crate::Type;
use crate::NamePath;
use serde::Deserialize;
use serde::Serialize;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VariantCase {
    pub name: String,
    pub ty: Option<Type>,
}

impl VariantCase {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
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
    pub name: NamePath,
    pub tag_type: Type,

    pub cases: Vec<VariantCase>,
}
