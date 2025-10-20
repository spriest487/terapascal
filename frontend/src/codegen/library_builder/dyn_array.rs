use terapascal_ir::instruction_builder::InstructionBuilder;
use crate::codegen::builder::Builder;
use crate::codegen::ir;
use crate::codegen::library_builder::LibraryBuilder;

pub fn gen_dyn_array_funcs(lib: &mut LibraryBuilder, elem_ty: &ir::Type, array_class_id: ir::TypeDefID) {
    let get_mem_id = lib.instantiate_get_mem_func();
    let array_ty = array_class_id.to_class_ptr_type();

    let mut alloc_builder = Builder::new(lib);

    alloc_builder.comment("bind params");
    alloc_builder.bind_param(ir::Type::any(), "arr_ptr", false);
    alloc_builder.bind_param(ir::Type::I32, "len", false);
    alloc_builder.bind_param(ir::Type::any(), "src_arr_ptr", false);
    alloc_builder.bind_param(ir::Type::Nothing.ptr(), "default_val", false);
    alloc_builder.retain(ir::LocalID(0), &array_ty);
    alloc_builder.retain(ir::LocalID(2), &array_ty);

    alloc_builder.gen_dyn_array_alloc_body(elem_ty, array_class_id, get_mem_id);
    let alloc_body = alloc_builder.finish();

    let dyn_array_rtti =
        lib.metadata().get_dyn_array_class(elem_ty).expect("missing dynarray rtti for type");

    let alloc_debug_name = if lib.opts.debug {
        Some(format!(
            "dynarray alloc function for element type {}",
            lib.metadata().pretty_ty_name(elem_ty)
        ))
    } else {
        None
    };

    lib.insert_func(
        dyn_array_rtti.alloc,
        ir::Function::Local(ir::FunctionDef {
            debug_name: alloc_debug_name,
            sig: ir::FunctionSig {
                param_tys: vec![ir::Type::any(), ir::Type::I32, ir::Type::any(), ir::Type::Nothing.ptr()],
                return_ty: ir::Type::Nothing,
            },
            body: alloc_body,
        }),
    );

    let mut length_builder = Builder::new(lib);
    length_builder.bind_return();
    length_builder.bind_param(ir::Type::any(), "arr_ptr", false);
    length_builder.retain(ir::LocalID(1), &array_ty);
    length_builder.gen_dyn_array_length_body(array_class_id);

    let length_body = length_builder.finish();

    let length_debug_name = if lib.opts.debug {
        Some(format!(
            "dynarray length function for element type {}",
            lib.metadata().pretty_ty_name(elem_ty)
        ))
    } else {
        None
    };

    lib.insert_func(
        dyn_array_rtti.length,
        ir::Function::Local(ir::FunctionDef {
            debug_name: length_debug_name,
            sig: ir::FunctionSig {
                param_tys: vec![ir::Type::any()],
                return_ty: ir::Type::I32,
            },
            body: length_body,
        }),
    );
}

pub fn gen_dyn_array_runtime_type(
    lib: &mut LibraryBuilder,
    elem_type: &ir::Type,
    array_class_id: ir::TypeDefID,
) {
    let array_ref_ty = array_class_id.to_class_ptr_type();
    let array_struct_ty = array_class_id.to_struct_type();

    let dtor_id = lib.metadata_mut().insert_func(None);
    lib.metadata_mut().insert_dtor(array_class_id, dtor_id);

    let free_mem_id = lib.instantiate_free_mem_func();

    let mut dtor_builder = Builder::new(lib);
    dtor_builder.bind_param(array_struct_ty.clone().ptr(), "self", true);
    dtor_builder.gen_dyn_array_dtor_body(elem_type, array_class_id, free_mem_id);

    let dtor_body = dtor_builder.finish();

    let debug_name = if lib.opts.debug {
        let array_ref_ty_name = lib.metadata().pretty_ty_name(&array_ref_ty).into_owned();
        Some(format!(
            "<generated dynarray releaser for {}>",
            array_ref_ty_name
        ))
    } else {
        None
    };

    lib.insert_func(
        dtor_id,
        ir::Function::Local(ir::FunctionDef {
            debug_name,
            sig: ir::FunctionSig {
                return_ty: ir::Type::Nothing,
                param_tys: vec![array_struct_ty.clone().ptr()],
            },
            body: dtor_body,
        }),
    );

    let weak_ty = ir::Type::RcWeakPointer(ir::VirtualTypeID::Class(array_class_id));
    lib.gen_runtime_type(&weak_ty);
}
