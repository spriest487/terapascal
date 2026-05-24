use crate::NamePath;
use crate::StructIdentity;
use crate::TagInfo;
use crate::Type;
use crate::FieldID;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeMap;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct StructDef {
    pub identity: StructIdentity,
    pub tags: Vec<TagInfo>,

    pub fields: BTreeMap<FieldID, StructFieldDef>,
    pub layout: StructLayout,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum StructLayout {
    Default,
    Packed,
}

impl StructDef {
    pub fn find_field(&self, name: &str) -> Option<FieldID> {
        self.fields.iter().find_map(|(id, field)| {
            let field_name = field.name.as_ref()?;
            if field_name == name {
                Some(*id)
            } else {
                None
            }
        })
    }

    pub fn get_field(&self, id: FieldID) -> Option<&StructFieldDef> {
        self.fields.get(&id)
    }
    
    pub fn is_generic(&self) -> bool {
        match self.name() {
            Some(name) => {
                name.is_generic()
            }
            None => false,
        }
    }
    
    pub fn is_class(&self) -> bool {
        matches!(self.identity, StructIdentity::Class(..))
    }
    
    pub fn is_equivalent_def(&self, other: &Self) -> bool {
        if self.identity != other.identity || self.fields.len() != other.fields.len() {
            return false;
        }
        
        for (field_id, field_def) in &self.fields {
            let Some(other_field) = other.get_field(*field_id) else {
                return false;
            };

            if field_def.ty != other_field.ty {
                return false;
            }
        }

        true
    }

    pub fn new(identity: StructIdentity, layout: StructLayout) -> Self {
        Self {
            identity,
            fields: BTreeMap::new(),
            tags: Vec::new(),
            layout,
        }
    }

    pub fn name(&self) -> Option<&NamePath> {
        self.identity.name()
    }
    
    fn next_field_id(&self) -> FieldID {
        self
            .fields
            .keys()
            .max_by_key(|id| id.0)
            .map(|id| FieldID(id.0 + 1))
            .unwrap_or(FieldID(0))
    }

    pub fn with_field(mut self, name: impl Into<String>, ty: Type) -> Self {
        let id = self.next_field_id();
        self.fields.insert(id, StructFieldDef::new(ty).with_name(name));
        self
    }

    pub fn with_fields(mut self, fields: BTreeMap<FieldID, StructFieldDef>) -> Self {
        self.fields.extend(fields);
        self
    }
    
    pub fn with_tags(mut self, tags: impl IntoIterator<Item=TagInfo>) -> Self {
        self.tags.extend(tags.into_iter());
        self
    }
}

impl fmt::Display for StructDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.identity {
            StructIdentity::Class(name) | StructIdentity::Record(name) => {
                write!(f, "{}", name)
            },
            StructIdentity::SetFlags { bits, .. } => {
                write!(f, "set<{bits}>")
            },
            StructIdentity::ClosureObject(identity) => {
                write!(f, "closure of function type {} ({})", identity.sig, identity.id)
            },
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct StructFieldDef {
    pub name: Option<String>,
    pub ty: Type,
}

impl StructFieldDef {
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            name: None,
        }
    }

    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }
}