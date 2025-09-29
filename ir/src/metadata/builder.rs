mod types;
mod functions;
mod rtti;

use std::iter;
use std::ops::Deref;
use std::sync::Arc;
use crate::dep_sort::sort_defs;
use crate::{FunctionID, InterfaceID, SetAliasID};
use crate::StringID;
use crate::Metadata;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDefID;
use crate::VariableID;
use linked_hash_map::LinkedHashMap;

#[derive(Debug)]
pub struct MetadataBuilder {
    next_type_id: TypeDefID,
    next_variable_id: VariableID,
    next_function_id: FunctionID,
    next_string_id: StringID,
    next_iface_id: InterfaceID,
    next_set_id: SetAliasID,
    
    refs: Vec<Arc<Metadata>>,
    
    metadata: Metadata,
}

impl MetadataBuilder {
    pub fn new() -> Self {
        Self::with_refs([])
    }
    
    pub fn with_refs(refs: impl IntoIterator<Item=Arc<Metadata>>) -> Self {
        let refs: Vec<_> = refs.into_iter().collect();
        
        let mut next_type_id = TypeDefID(1);
        let mut next_iface_id = InterfaceID(1);
        let mut next_variable_id = VariableID(1);
        let mut next_function_id = FunctionID(1);
        let mut next_string_id = StringID(1);
        let mut next_set_id = SetAliasID(1);
        
        for ref_metadata in &refs {
            if let Some(max_id) = ref_metadata.type_decls.keys().max() {
                next_type_id.0 = max_id.0 + 1;
            }
            if let Some(max_id) = ref_metadata.ifaces.keys().max() {
                next_iface_id.0 = max_id.0 + 1;
            }
            if let Some(max_id) = ref_metadata.variables.keys().max() {
                next_variable_id.0 = max_id.0 + 1;
            }
            if let Some(max_id) = ref_metadata.functions.keys().max() {
                next_function_id.0 = max_id.0 + 1;
            }
            if let Some(max_id) = ref_metadata.string_literals.keys().max() {
                next_string_id.0 = max_id.0 + 1;
            }
            if let Some(max_id) = ref_metadata.set_aliases.keys().max() {
                next_set_id.0 = max_id.0 + 1;
            }
        }

        Self {
            next_type_id,
            next_iface_id,
            next_variable_id,
            next_function_id,
            next_string_id,
            next_set_id,
            
            refs,

            metadata: Metadata::new()
        }
    }
    
    pub fn new_variable(&mut self, ty: Type) -> VariableID {
        let id = self.next_variable_id;

        while let Some(..) = self.variables.get(&self.next_variable_id) {
            self.next_variable_id.0 += 1;
        }
        self.metadata.variables.insert(id, ty);

        self.next_variable_id.0 += 1;
        
        id
    }
    
    pub fn build(mut self) -> Metadata {
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

        let sorted_defs = sort_defs(defs, self.as_ref());

        self.metadata.type_decls = decls;
        for (id, def) in sorted_defs {
            self.metadata.type_decls.insert(id, TypeDecl::Def(def));
        }
    }
    
    fn all_metadata(&self) -> impl Iterator<Item=&Metadata> {
        iter::once(&self.metadata)
            .chain(self.refs.iter()
                .map(|metadata| metadata.as_ref()))
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

impl AsRef<Metadata> for MetadataBuilder {
    fn as_ref(&self) -> &Metadata {
        &self.metadata
    }
}

impl Deref for MetadataBuilder {
    type Target = Metadata;

    fn deref(&self) -> &Self::Target {
        &self.metadata
    }
}
