use crate::codegen::builder::IRBuilder;
use crate::codegen::library_builder::LibraryBuilder;
use std::collections::HashMap;
use terapascal_ir as ir;
use terapascal_ir::generic::instantiate_struct_def;
use terapascal_ir::InstructionBuilder;
use terapascal_ir::MetadataSource;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct RcMethodInfo {
    pub retain_elements: Option<ir::FunctionID>,
    pub release_elements: Option<ir::FunctionID>,
}

impl RcMethodInfo {
    pub fn gen_for_type(lib: &mut LibraryBuilder, ty: &ir::Type) -> Self {
        let mut rc_type_cache = HashMap::new();
        
        let retain = gen_retain_func(lib, ty, &mut rc_type_cache);
        let release = gen_release_func(lib, ty, &mut rc_type_cache);
        
        Self {
            retain_elements: retain,
            release_elements: release,
        }
    }
}

fn gen_release_func(
    lib: &mut LibraryBuilder,
    ty: &ir::Type,
    rc_type_cache: &mut HashMap<ir::Type, bool>,
) -> Option<ir::FunctionID> {
    if !has_rc_members_rec(ty, lib, rc_type_cache) {
        return None;
    }

    let body = gen_release_body(lib, ty);

    let debug_name = lib
        .opts().debug
        .then(|| format!("<generated RC release for {}>", lib.metadata().pretty_type_name(ty)));

    let func_id = create_rc_func(lib, ty, body, debug_name);

    Some(func_id)
}

// generate deep release/retain funcs for non-RC types, 
// including the internal structures of RC types
fn gen_release_body(lib: &mut LibraryBuilder, ty: &ir::Type) -> ir::InstructionList {
    let mut release_builder = IRBuilder::new(lib);

    let target_ref_arg = ir::ArgID(0);
    release_builder.bind_ref_param(target_ref_arg, ty.clone(), "target");

    release_builder.visit_deep(
        target_ref_arg.to_deref(),
        ty,
        |builder, element_type, element_ref| {
            if element_type.is_object() {
                builder.release(element_ref, element_type.is_weak(), ir::Ref::Discard);
                true
            } else {
                false
            }
        },
    );

    release_builder.finish()
}

fn gen_retain_func(
    lib: &mut LibraryBuilder,
    ty: &ir::Type,
    rc_type_cache: &mut HashMap<ir::Type, bool>,
) -> Option<ir::FunctionID> {
    if !has_rc_members_rec(ty, lib, rc_type_cache) {
        return None;
    }
    
    let body = gen_retain_body(lib, ty);

    let debug_name = lib
        .opts().debug
        .then(|| format!("<generated RC retain for {}>", lib.metadata().pretty_type_name(ty)));

    let func_id = create_rc_func(lib, ty, body, debug_name);

    Some(func_id)
}

fn gen_retain_body(lib: &mut LibraryBuilder, ty: &ir::Type) -> ir::InstructionList {
    let mut retain_builder = IRBuilder::new(lib);
    
    let target_ref_arg = ir::ArgID(0);

    retain_builder.bind_ref_param(target_ref_arg, ty.clone(), "target");

    retain_builder.visit_deep(
        target_ref_arg.to_deref(),
        ty,
        |builder, element_type, element_ref| {
            if element_type.is_object() {
                builder.retain(element_ref, element_type.is_weak());
                true
            } else {
                false
            }
        },
    );

    retain_builder.finish()
}

fn create_rc_func(
    lib: &mut LibraryBuilder,
    ty: &ir::Type,
    body: ir::InstructionList,
    debug_name: Option<String>
) -> ir::FunctionID {
    let sig = ir::FunctionSig::new([ty.clone().temp_ref()], ir::Type::Nothing);

    let func_id = lib.metadata_mut().insert_func(None, sig.clone(), false, []);
    lib.insert_function(func_id, ir::Function::new_local_def(debug_name, Vec::new(), sig, body));

    func_id
}

fn has_rc_members_rec(
    ty: &ir::Type,
    lib: &mut LibraryBuilder,
    cache: &mut HashMap<ir::Type, bool>
) -> bool {
    if let Some(result) = cache.get(ty) {
        return *result;
    }

    let result = match ty {
        ir::Type::Struct { id, args, .. } => {
            let def = lib
                .metadata()
                .get_struct_def(*id)
                .unwrap_or_else(|| panic!("has_rc_members_rec: missing def for struct {}", id))
                .clone();
            let def = instantiate_struct_def(&def, args);

            let mut any_rc = false;
            for (_, field_def) in &def.fields {
                if field_def.ty.is_object() || has_rc_members_rec(&field_def.ty, lib, cache) {
                    any_rc = true;
                    break;
                }
            }
            any_rc
        }

        ir::Type::Variant(id) => {
            let def = lib
                .metadata()
                .get_variant_def(*id)
                .unwrap_or_else(|| panic!("has_rc_members_rec: missing def for variant {}", id))
                .clone();
            
            let mut any_rc = false;
            for case in &def.cases {
                let Some(case_ty) = &case.ty else {
                    continue;
                };

                if case_ty.is_object() || has_rc_members_rec(case_ty, lib, cache) {
                    any_rc = true;
                    break;
                }
            }
            any_rc
        }
        
        ir::Type::Array { element, .. } => {
            if element.is_object() {
                true
            } else {
                has_rc_members_rec(element, lib, cache)
            }
        }

        _ => false,
    };
    
    cache.insert(ty.clone(), result);
    result
}