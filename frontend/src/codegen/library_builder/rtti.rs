use crate::codegen::builder::Builder;
use crate::codegen::ir;
use crate::codegen::library_builder::LibraryBuilder;
use terapascal_ir::instruction_builder::InstructionBuilder;

pub fn gen_release_func(lib: &mut LibraryBuilder, ty: &ir::Type) -> Option<ir::FunctionID> {
    let body = gen_release_body(lib, ty)?;

    let debug_name = lib
        .opts().debug
        .then(|| format!("<generated RC release for {}>", lib.metadata().pretty_ty_name(ty)));

    let func_id = create_rc_func(lib, ty, body, debug_name);

    Some(func_id)
}

// generate deep release/retain funcs for non-RC types, 
// including the internal structures of RC types
fn gen_release_body(lib: &mut LibraryBuilder, ty: &ir::Type) -> Option<Vec<ir::Instruction>> {
    if !ty.is_rc() {
        let mut release_builder = Builder::new(lib);
        release_builder.bind_param(ir::LocalID(0), ty.clone().ptr(), "target", true);
        let target_ref = ir::Ref::Local(ir::LocalID(0)).to_deref();

        let released_any = release_builder.release_deep(target_ref, ty);

        let body = release_builder.finish();
        if released_any && !body.is_empty() {
            Some(body)
        } else {
            None
        }
    } else {
        None
    }
}

pub fn gen_retain_func(lib: &mut LibraryBuilder, ty: &ir::Type) -> Option<ir::FunctionID> {
    let body = gen_retain_body(lib, ty)?;

    let debug_name = lib
        .opts().debug
        .then(|| format!("<generated RC retain for {}>", lib.metadata().pretty_ty_name(ty)));

    let func_id = create_rc_func(lib, ty, body, debug_name);

    Some(func_id)
}

fn gen_retain_body(lib: &mut LibraryBuilder, ty: &ir::Type) -> Option<Vec<ir::Instruction>> {
    if !ty.is_rc() {
        let mut retain_builder = Builder::new(lib);
        retain_builder.bind_param(ir::LocalID(0), ty.clone().ptr(), "target", true);
        let target_ref = ir::Ref::Local(ir::LocalID(0)).to_deref();

        let retained_any = retain_builder.retain_deep(target_ref, ty);

        let body = retain_builder.finish();
        if retained_any && !body.is_empty() {
            Some(body)
        } else {
            None
        }
    } else {
        None
    }
}

fn create_rc_func(
    lib: &mut LibraryBuilder,
    ty: &ir::Type,
    body: Vec<ir::Instruction>,
    debug_name: Option<String>
) -> ir::FunctionID {
    let func_id = lib.metadata_mut().insert_func(None);

    let sig = ir::FunctionSig::new([ty.clone().ptr()], ir::Type::Nothing);
    lib.insert_func(func_id, ir::Function::new_local_def(debug_name, sig, body));

    func_id
}
