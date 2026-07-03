use crate::FunctionID;
use crate::FunctionParamInfo;
use crate::MethodID;
use crate::NamePath;
use crate::TagInfo;
use crate::Type;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Method {
    pub name: String,
    pub return_ty: Type,
    pub params: Vec<FunctionParamInfo>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InterfaceDef {
    pub name: NamePath,
    pub methods: Vec<Method>,
    
    pub tags: Vec<TagInfo>,
}

impl InterfaceDef {
    pub fn new(name: impl Into<NamePath>, methods: impl Into<Vec<Method>>) -> Self {
        Self {
            name: name.into(),
            methods: methods.into(),
            tags: Vec::new(),
        }
    }

    pub fn method_index(&self, name: &str) -> Option<MethodID> {
        self.methods
            .iter()
            .position(|m| m.name.as_str() == name)
            .map(MethodID)
    }

    pub fn get_method(&self, id: MethodID) -> Option<&Method> {
        self.methods.get(id.0)
    }
    
    pub fn with_tags(mut self, tags: impl IntoIterator<Item=TagInfo>) -> Self {
        self.tags.extend(tags.into_iter());
        self
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InterfaceImpl {
    // method index -> method impl
    pub methods: HashMap<MethodID, FunctionID>,
}

impl InterfaceImpl {
    pub fn new(method_count: usize) -> Self {
        Self {
            methods: HashMap::with_capacity(method_count),
        }
    }
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InterfaceDecl {
    Forward(NamePath),
    Def(InterfaceDef),
}

impl InterfaceDecl {
    pub fn name(&self) -> &NamePath {
        match self {
            InterfaceDecl::Def(def) => &def.name,
            InterfaceDecl::Forward(name) => name,
        }
    }
}
