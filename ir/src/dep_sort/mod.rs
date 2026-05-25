use crate::metadata::Metadata;
use crate::MetadataSource;
use crate::StructDef;
use crate::Type;
use crate::TypeDef;
use crate::TypeDefID;
use crate::VariantDef;
use linked_hash_map::LinkedHashMap;
use std::collections::HashMap;
use std::collections::HashSet;
use topological_sort::TopologicalSort;

// sort list of type defs to resolve deep structural dependencies on other defs.
// struct, variant and static array fields count as structural dependencies (and are
// guaranteed from the pascal end to be non-recursive, because they must be declared
// in order).
// this will panic if any defs depend on themselves.
pub fn sort_defs<Defs>(defs: Defs, metadata: &Metadata) -> LinkedHashMap<TypeDefID, TypeDef>
where
    Defs: IntoIterator<Item = (TypeDefID, TypeDef)>,
{
    let mut sort = TopologicalSort::new();

    let mut defs: HashMap<TypeDefID, TypeDef> = defs.into_iter().collect();
    
    for (id, def) in defs.iter() {
        sort.insert(*id);
        
        for dep in find_deps(def, metadata) {
            sort.add_dependency(*id, dep);
            
            if sort.peek().is_none() {
                panic!("circular dependency in definitions: {} -> {}", id, dep);
            }
        }
    }

    let sorted: Vec<_> = sort
        .map(|id| (id, defs.remove(&id).unwrap()))
        .collect();
    
    sorted
        .into_iter()
        .rev()
        .collect()
}

pub fn find_deps(def: &TypeDef, metadata: &Metadata) -> HashSet<TypeDefID> {
    let mut deps = HashSet::new();

    match def {
        TypeDef::Struct(struct_def) => {
            add_struct_deps(struct_def, &mut deps, metadata);
        }

        TypeDef::Variant(variant_def) => {
            add_variant_deps(variant_def, &mut deps, metadata);
        }
    }

    deps
}

fn add_struct_deps(struct_def: &StructDef, deps: &mut HashSet<TypeDefID>, metadata: &Metadata) {
    for (_, field) in &struct_def.fields {
        add_dep(&field.ty, deps, metadata);
    }
}

fn add_variant_deps(variant_def: &VariantDef, deps: &mut HashSet<TypeDefID>, metadata: &Metadata) {
    for case in &variant_def.cases {
        if let Some(case_ty) = &case.ty {
            add_dep(case_ty, deps, metadata);
        }
    }
}

fn add_dep(ty: &Type, deps: &mut HashSet<TypeDefID>, metadata: &Metadata) {
    match ty {
        Type::Variant(id) => {
            deps.insert(id.def_id);

            let def = metadata.get_variant_def(id.def_id).unwrap();
            add_variant_deps(def, deps, metadata);

            for arg in &id.args {
                add_dep(arg, deps, metadata);
            }
        }

        Type::Struct(id) => {
            deps.insert(id.def_id);

            let def = metadata.get_struct_def(id.def_id).unwrap();
            add_struct_deps(def, deps, metadata);

            for arg in &id.args {
                add_dep(arg, deps, metadata);
            }
        }

        Type::Array { element, .. } => {
            add_dep(element, deps, metadata);
        }

        _ => {
            // no structural deps
        }
    }
}
