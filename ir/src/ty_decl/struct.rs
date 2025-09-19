use crate::FieldID;
use crate::NamePath;
use crate::StructIdentity;
use crate::Type;
use linked_hash_map::LinkedHashMap;
use serde::Deserialize;
use serde::Serialize;
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Struct {
    pub identity: StructIdentity,
    pub fields: LinkedHashMap<FieldID, StructFieldDef>,
}

impl Struct {
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
    
    pub fn is_equivalent_def(&self, other: &Self) -> bool {
        if self.identity != other.identity || self.fields.len() != other.fields.len() {
            return false;
        }
        
        for (field_id, field_def) in &self.fields {
            let Some(other_field) = other.get_field(*field_id) else {
                return false;
            };

            if field_def.ty != other_field.ty || field_def.rc != other_field.rc {
                return false;
            }
        }

        true
    }

    pub fn new(identity: StructIdentity) -> Self {
        Self {
            identity,
            fields: LinkedHashMap::new(),
        }
    }

    pub fn name(&self) -> Option<&NamePath> {
        match &self.identity {
            StructIdentity::Class(name) 
            | StructIdentity::Record(name) => Some(name),
            
            StructIdentity::Closure(..) 
            | StructIdentity::Array(..) 
            | StructIdentity::DynArray(..) 
            | StructIdentity::SetFlags { .. } => None,
        }
    }

    pub fn with_field(mut self, name: impl Into<String>, ty: Type, rc: bool) -> Self {
        let id = self
            .fields
            .keys()
            .max_by_key(|id| id.0)
            .map(|id| FieldID(id.0 + 1))
            .unwrap_or(FieldID(0));

        self.fields.insert(
            id,
            StructFieldDef {
                name: Some(name.into()),
                ty,
                rc,
            },
        );

        self
    }

    pub fn with_fields(mut self, fields: HashMap<FieldID, StructFieldDef>) -> Self {
        self.fields.extend(fields);
        self
    }
}

impl fmt::Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.identity {
            StructIdentity::Array(ty, dim) => write!(f, "array[{}] of {}", dim, ty),
            StructIdentity::DynArray(ty) => write!(f, "array of {}", ty),
            StructIdentity::Class(name) | StructIdentity::Record(name) => write!(f, "{}", name),
            StructIdentity::SetFlags { bits, .. } => write!(f, "set<{bits}>"),
            StructIdentity::Closure(identity) => write!(
                f,
                "closure of function type {} ({})",
                identity.virt_func_ty, identity.id
            ),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct StructFieldDef {
    pub name: Option<String>,
    pub ty: Type,
    pub rc: bool,
}
