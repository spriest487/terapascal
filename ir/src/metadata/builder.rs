mod types;
mod functions;
mod rtti;

use crate::dep_sort::sort_defs;
use crate::metadata::vars::ConstInfo;
use crate::FunctionID;
use crate::InterfaceID;
use crate::Metadata;
use crate::MetadataCollection;
use crate::NamePath;
use crate::StringID;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDefID;
use crate::Value;
use crate::VariableID;
use crate::VariableInfo;
use crate::EMPTY_STRING_ID;
use crate::RESERVED_STRINGS;
use crate::RESERVED_TYPES;
use linked_hash_map::LinkedHashMap;
use std::iter;
use std::sync::Arc;

#[derive(Debug)]
pub struct MetadataBuilder {
    next_type_id: TypeDefID,
    next_variable_id: VariableID,
    next_function_id: FunctionID,
    next_string_id: StringID,
    next_iface_id: InterfaceID,
    
    refs: Vec<Arc<Metadata>>,
    
    metadata: Metadata,
}

impl MetadataBuilder {
    pub fn new() -> Self {
        Self::with_refs([])
    }

    pub fn with_refs(refs: impl IntoIterator<Item=Arc<Metadata>>) -> Self {
        let refs: Vec<_> = refs.into_iter().collect();
        
        let first_user_type = RESERVED_TYPES
            .iter()
            .max()
            .map(|id| id.0 + 1)
            .unwrap_or(1);

        let first_user_string = RESERVED_STRINGS
            .iter()
            .max()
            .map(|id| id.0 + 1)
            .unwrap_or(1);
        
        let mut next_type_id = TypeDefID(first_user_type);
        let mut next_iface_id = InterfaceID(1);
        let mut next_variable_id = VariableID(1);
        let mut next_function_id = FunctionID(1);
        let mut next_string_id = StringID(first_user_string);

        for ref_metadata in &refs {
            if let Some(max_id) = ref_metadata.type_decls.keys().max() {
                next_type_id.0 = usize::max(max_id.0 + 1, next_type_id.0);
            }
            if let Some(max_id) = ref_metadata.ifaces.keys().max() {
                next_iface_id.0 = usize::max(max_id.0 + 1, next_iface_id.0);
            }
            if let Some(max_id) = ref_metadata.variables.keys().max() {
                next_variable_id.0 = usize::max(max_id.0 + 1, next_variable_id.0);
            }
            if let Some(max_id) = ref_metadata.function_info.keys().max() {
                next_function_id.0 = usize::max(max_id.0 + 1, next_function_id.0);
            }
            if let Some(max_id) = ref_metadata.string_literals.keys().max() {
                next_string_id.0 = usize::max(max_id.0 + 1, next_string_id.0);
            }
        }

        let mut metadata = Metadata::new();
        metadata
            .string_literals
            .insert(EMPTY_STRING_ID, String::new());

        for reserved_id in RESERVED_TYPES {
            metadata.type_decls.insert(reserved_id, TypeDecl::Reserved);
        }

        Self {
            next_type_id,
            next_iface_id,
            next_variable_id,
            next_function_id,
            next_string_id,
            
            refs,

            metadata,
        }
    }
    
    pub fn metadata(&self) -> &Metadata {
        &self.metadata
    }
    
    pub fn new_variable(&mut self, name: Option<NamePath>, ty: Type) -> VariableID {
        let id = self.next_variable_id;

        while let Some(..) = self.metadata.variables.get(&self.next_variable_id) {
            self.next_variable_id.0 += 1;
        }

        self.metadata.variables.insert(id, VariableInfo {
            name,
            r#type: ty,
        });

        self.next_variable_id.0 += 1;
        
        id
    }

    pub fn new_const(&mut self, name: NamePath, value: Value) {
        self.metadata.constants.insert(name.clone(), ConstInfo {
            name,
            value,
        });
    }
    
    pub fn build(mut self) -> Metadata {
        // remove reserved type decls - if they weren't defined, they must either be unused
        // or defined in another library
        let reserved_type_ids: Vec<_> = self.metadata.type_decls
            .iter()
            .filter_map(|(id, decl)| {
                match decl {
                    TypeDecl::Reserved => Some(*id),
                    _ => None,
                }
            })
            .collect();
        for type_id in reserved_type_ids {
            self.metadata.type_decls.remove(&type_id);
        }

        self.sort_type_defs_by_deps();
        
        self.metadata
    }

    // hack: we don't always end up with types properly ordered by structural dependencies
    // as a result of the order we encounter types in, so this gets called to sort them before
    // finishing the module (assuming backends expect the types to be ordered e.g. like in C)
    fn sort_type_defs_by_deps(&mut self) {
        let mut unsorted = self.metadata.type_decls.clone();

        // remove all defs into a separate collection
        let mut defs = Vec::new();
        let mut decls = LinkedHashMap::new();

        while let Some((id, decl)) = unsorted.pop_front() {
            match decl {
                TypeDecl::Reserved => {
                    decls.insert(id, TypeDecl::Reserved);
                },
                TypeDecl::Forward(name) => {
                    decls.insert(id, TypeDecl::Forward(name));
                },

                TypeDecl::Def(def) => {
                    defs.push((id, def));
                },
            }
        }

        let sorted_defs = sort_defs(defs, self);

        self.metadata.type_decls = decls;
        for (id, def) in sorted_defs {
            self.metadata.type_decls.insert(id, TypeDecl::Def(def));
        }
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringID {
        if let Some(existing) = self.all_metadata().find_map(|m| m.find_string_id(s)) {
            return existing;
        }

        let next_id = self.next_string_id;
        self.metadata.string_literals.insert(next_id, s.to_string());
        
        self.next_string_id.0 += 1;
        next_id
    }
}

impl MetadataCollection for MetadataBuilder {
    fn all_metadata(&self) -> impl Iterator<Item=&Metadata> {
        iter::once(&self.metadata)
            .chain(self.refs
                .iter()
                .rev()
                .map(|metadata| metadata.as_ref()))
    }
}