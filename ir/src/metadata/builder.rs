mod types;
mod functions;
mod rtti;

use crate::dep_sort::sort_defs;
use crate::FieldID;
use crate::FunctionID;
use crate::IRFormatter;
use crate::InterfaceID;
use crate::Metadata;
use crate::MethodID;
use crate::Ref;
use crate::SetAliasID;
use crate::StringID;
use crate::Type;
use crate::TypeDecl;
use crate::TypeDefID;
use crate::Value;
use crate::VariableID;
use crate::RESERVED_STRINGS;
use crate::RESERVED_TYPES;
use linked_hash_map::LinkedHashMap;
use std::borrow::Cow;
use std::fmt;
use std::iter;
use std::sync::Arc;

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
    
    pub fn metadata(&self) -> &Metadata {
        &self.metadata
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> Cow<'_, str> {
        self.metadata.pretty_ty_name(ty)
    }
    
    pub fn new_variable(&mut self, ty: Type) -> VariableID {
        let id = self.next_variable_id;

        while let Some(..) = self.metadata.variables.get(&self.next_variable_id) {
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

    fn find_in_self_or_refs<'a, T, F>(&'a self, f: F) -> Option<T>
    where
        F: Fn(&'a Metadata) -> Option<T>,
        T: 'a
    {
        if let Some(result) = f(&self.metadata) {
            return Some(result);
        }

        for ref_metadata in &self.refs {
            if let Some(result) = f(ref_metadata.as_ref()) {
                return Some(result);
            }
        }

        None
    }

    fn iter_in_self_or_refs<'a, T, F, Iter>(&'a self, f: F) -> impl Iterator<Item=T>
    where
        F: Fn(&'a Metadata) -> Iter + 'a,
        Iter: Iterator<Item=T> + 'a, 
        T: 'a
    {
        let self_results = f(&self.metadata);

        let dep_results = self.refs
            .iter()
            .flat_map(move |ref_metadata| f(ref_metadata));

        self_results.chain(dep_results)
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

        let sorted_defs = sort_defs(defs, &self.metadata);

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


impl IRFormatter for MetadataBuilder {
    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result {
        self.metadata.format_type(ty, f)
    }

    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result {
        self.metadata.format_val(val, f)
    }

    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result {
        self.metadata.format_ref(r, f)
    }

    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result {
        self.metadata.format_field(of_ty, field, f)
    }

    fn format_method(&self, iface: InterfaceID, method: MethodID, f: &mut dyn fmt::Write) -> fmt::Result {
        self.metadata.format_method(iface, method, f)
    }

    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut dyn fmt::Write) -> fmt::Result {
        self.metadata.format_variant_case(of_ty, tag, f)
    }
}
