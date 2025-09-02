use crate::codegen::builder::Builder;
use crate::codegen::ir;
use crate::codegen::library_builder::LibraryBuilder;

pub fn gen_dyn_array_funcs(lib: &mut LibraryBuilder, elem_ty: &ir::Type, struct_id: ir::TypeDefID) {
    let mut alloc_builder = Builder::new(lib);
    gen_dyn_array_alloc_func(&mut alloc_builder, elem_ty, struct_id);
    let alloc_body = alloc_builder.finish();

    let dyn_array_rtti =
        lib.metadata().get_dynarray_runtime_type(elem_ty).expect("missing dynarray rtti for type");

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
                param_tys: vec![ir::Type::any(), ir::Type::I32, ir::Type::any(), ir::Type::any()],
                return_ty: ir::Type::Nothing,
            },
            body: alloc_body,
        }),
    );

    let mut length_builder = Builder::new(lib);
    gen_dyn_array_length_func(&mut length_builder, struct_id);
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

fn gen_dyn_array_alloc_func(builder: &mut Builder, elem_ty: &ir::Type, struct_id: ir::TypeDefID) {
    let array_ref_ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(struct_id));
    let el_ptr_ty = elem_ty.clone().ptr();

    builder.comment("bind params");
    let arr_arg = ir::LocalID(0);
    let len_arg = ir::LocalID(1);
    let src_arr_arg = ir::LocalID(2);
    let default_val_arg = ir::LocalID(3);
    builder.bind_param(arr_arg, ir::Type::any(), "arr_ptr", false);
    builder.bind_param(len_arg, ir::Type::I32, "len", false);
    builder.bind_param(src_arr_arg, ir::Type::any(), "src_arr_ptr", false);
    builder.bind_param(
        default_val_arg,
        ir::Type::Nothing.ptr(),
        "default_val",
        false,
    );

    builder.comment("retain the refs to the array params");
    builder.retain(ir::Ref::Local(ir::LocalID(0)), &ir::Type::any());
    builder.retain(ir::Ref::Local(ir::LocalID(2)), &ir::Type::any());

    builder.comment("cast the array params to this array type");
    let arr = builder.local_temp(array_ref_ty.clone());
    let src_arr = builder.local_temp(array_ref_ty.clone());
    builder.cast(
        arr.clone(),
        ir::Ref::Local(ir::LocalID(0)),
        array_ref_ty.clone(),
    );
    builder.cast(
        src_arr.clone(),
        ir::Ref::Local(ir::LocalID(2)),
        array_ref_ty.clone(),
    );

    let default_el_ptr = builder.local_temp(el_ptr_ty.clone());
    builder.cast(default_el_ptr.clone(), default_val_arg, el_ptr_ty.clone());

    builder.comment("el_len := sizeof(elem_ty)");
    let el_len = builder.local_temp(ir::Type::I32);
    builder.size_of(el_len.clone(), elem_ty.clone());

    builder.comment("data_len := el_len * len");
    let data_len = builder.local_temp(ir::Type::I32);
    builder.mul(data_len.clone(), el_len.clone(), len_arg);

    builder.comment("data = GetMem(data_len) as ^elem_ty");
    let data_mem = builder.local_temp(ir::Type::U8.ptr());
    builder.get_mem(data_len, data_mem.clone());
    let data = builder.local_temp(el_ptr_ty.clone());
    builder.cast(data.clone(), data_mem, el_ptr_ty.clone());

    builder.comment("iteration counter for initializing elements");
    let counter = builder.local_temp(ir::Type::I32);
    builder.mov(counter.clone(), ir::Value::LiteralI32(0));

    builder.comment("loop break flag we use in a couple of places later");
    let done = builder.local_temp(ir::Type::Bool);

    builder.comment("copy elements from copied array");

    let skip_copy_label = builder.alloc_label();
    builder.comment("skip copying from source if copy_from is null");
    let src_is_null = builder.eq_to_val(src_arr.clone(), ir::Value::LiteralNull);
    builder.jmpif(skip_copy_label, src_is_null);

    builder.comment("copy_len := copy_from->length");
    let src_len = builder.local_temp(ir::Type::I32);
    builder.field_val(
        src_len.clone(),
        src_arr.clone(),
        array_ref_ty.clone(),
        ir::DYNARRAY_LEN_FIELD,
        ir::Type::I32,
    );

    builder.scope(|builder| {
        let copy_count = builder.local_temp(ir::Type::I32);
        let copy_count_ok = builder.local_temp(ir::Type::Bool);

        builder.comment("copy_count := src_len");
        builder.mov(copy_count.clone(), src_len.clone());

        builder.comment(
            "if there are more elements in the source than we want, copy `len` elements instead",
        );
        let copy_count_ok_label = builder.alloc_label();

        builder.comment("copy_count_ok := copy_count <= len");
        builder.lte(copy_count_ok.clone(), copy_count.clone(), len_arg);

        builder.comment("if not copy_count_ok then copy_count := len");
        builder.jmpif(copy_count_ok_label, copy_count_ok);
        builder.mov(copy_count.clone(), len_arg);
        builder.label(copy_count_ok_label);

        builder.comment(
            "for `copy_count` iterations, copy the value at copy_src[counter] to copy_dst[counter]",
        );
        let copy_loop_label = builder.alloc_label();
        let copy_break_label = builder.alloc_label();

        builder.label(copy_loop_label);

        builder.comment("done := counter = copy_count");
        builder.comment("if done then break");
        builder.eq(done.clone(), counter.clone(), copy_count.clone());
        builder.jmpif(copy_break_label, done.clone());

        builder.scope(|builder| {
            let copy_dst = builder.local_temp(el_ptr_ty.clone());
            let copy_src = builder.local_temp(el_ptr_ty.clone());

            builder.comment("copy_dst := data + counter");
            builder.add(copy_dst.clone(), data.clone(), counter.clone());

            builder.comment("copy_src := src_arr->ptr + counter");
            builder.field_val(
                copy_src.clone(),
                src_arr.clone(),
                array_ref_ty.clone(),
                ir::DYNARRAY_PTR_FIELD,
                el_ptr_ty.clone(),
            );
            builder.add(copy_src.clone(), copy_src.clone(), counter.clone());

            builder.comment("copy_dst^ := copy_src^");
            builder.mov(copy_dst.clone().to_deref(), copy_src.to_deref());

            builder.retain(copy_dst.to_deref(), elem_ty);
        });

        builder.comment("counter += 1");
        builder.add(counter.clone(), counter.clone(), ir::Value::LiteralI32(1));

        builder.jmp(copy_loop_label);
        builder.label(copy_break_label);
    });

    builder.label(skip_copy_label);

    builder.comment("while counter < len, default init next element");
    let init_break_label = builder.alloc_label();
    let init_loop_label = builder.alloc_label();

    builder.label(init_loop_label);

    builder.comment("done := counter = len");
    builder.comment("if done then break");
    builder.eq(done.clone(), counter.clone(), len_arg);
    builder.jmpif(init_break_label, done);

    builder.scope(|builder| {
        builder.comment("data[counter] := default_val_ptr^");
        let el_ptr = builder.local_temp(el_ptr_ty.clone());
        builder.add(el_ptr.clone(), data.clone(), counter.clone());
        builder.mov(el_ptr.clone().to_deref(), default_el_ptr.clone().to_deref());

        builder.retain(el_ptr.to_deref(), elem_ty);
    });

    builder.comment("counter += 1");
    builder.add(counter.clone(), counter.clone(), ir::Value::LiteralI32(1));
    builder.jmp(init_loop_label);

    builder.label(init_break_label);

    builder.assign_field(
        arr.clone(),
        array_ref_ty.clone(),
        ir::DYNARRAY_LEN_FIELD,
        ir::Type::I32,
        len_arg,
    );
    builder.assign_field(arr, array_ref_ty, ir::DYNARRAY_PTR_FIELD, el_ptr_ty, data);
}

fn gen_dyn_array_length_func(builder: &mut Builder, struct_id: ir::TypeDefID) {
    let array_ref_ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(struct_id));

    builder.comment("bind and retain params");
    builder.bind_return();
    builder.bind_param(ir::LocalID(1), ir::Type::any(), "arr_ptr", false);
    builder.retain(ir::Ref::Local(ir::LocalID(1)), &ir::Type::any());

    builder.comment("cast pointer down to this array type");
    let arr = builder.local_temp(array_ref_ty.clone());
    builder.cast(
        arr.clone(),
        ir::Ref::Local(ir::LocalID(1)),
        array_ref_ty.clone(),
    );

    builder.comment("evaluate length field into return ref");
    builder.field_val(
        ir::RETURN_REF,
        arr,
        array_ref_ty,
        ir::DYNARRAY_LEN_FIELD,
        ir::Type::I32,
    );
}

pub fn gen_dyn_array_runtime_type(
    lib: &mut LibraryBuilder,
    elem_ty: &ir::Type,
    struct_id: ir::TypeDefID,
) {
    let array_ref_ty = ir::Type::RcPointer(ir::VirtualTypeID::Class(struct_id));
    let array_struct_ty = ir::Type::Struct(struct_id);

    let runtime_type = lib
        .metadata()
        .get_runtime_type(&array_struct_ty)
        .expect("rtti function ids for dynarray inner struct must exist");

    let mut builder = Builder::new(lib);

    builder.comment("%0 is the self arg, the pointer to the inner struct");
    builder.bind_param(ir::LocalID(0), array_struct_ty.clone().ptr(), "self", true);
    let self_arg = ir::Ref::Local(ir::LocalID(0)).to_deref();

    builder.comment("pointer to the length field of the dynarray object");
    let len_field_ptr = builder.local_temp(ir::Type::I32.ptr());

    builder.comment("pointer to the pointer field of the dynarray object");
    let arr_field_ptr = builder.local_temp(elem_ty.clone().ptr().ptr());

    builder.comment("u8 pointer type field to cast the array memory into to call FreeMem");
    let arr_mem_ptr = builder.local_temp(ir::Type::U8.ptr());

    builder.comment("iteration vars");
    let counter = builder.local_temp(ir::Type::I32);
    let has_more = builder.local_temp(ir::Type::Bool);
    let el_ptr = builder.local_temp(elem_ty.clone().ptr());

    let zero_elements = builder.local_temp(ir::Type::Bool);

    builder.comment("jump to loop end if counter == array len");
    let start_loop_label = builder.alloc_label();
    let end_loop_label = builder.alloc_label();

    let after_free = builder.alloc_label();

    builder.field(
        len_field_ptr.clone(),
        self_arg.clone(),
        array_struct_ty.clone(),
        ir::DYNARRAY_LEN_FIELD,
    );
    builder.field(
        arr_field_ptr.clone(),
        self_arg,
        array_struct_ty.clone(),
        ir::DYNARRAY_PTR_FIELD,
    );

    builder.comment("release every element");
    builder.mov(counter.clone(), ir::Value::LiteralI32(0));

    builder.label(start_loop_label);

    builder.comment("has_more := counter < array.length");

    builder.lt(
        has_more.clone(),
        counter.clone(),
        len_field_ptr.clone().to_deref(),
    );

    builder.comment("if not has_more then break");
    let at_end = builder.not_to_val(has_more);
    builder.jmpif(end_loop_label, at_end);

    builder.comment("release arr[counter]");
    builder.add(
        el_ptr.clone(),
        arr_field_ptr.clone().to_deref(),
        counter.clone(),
    );
    builder.release(el_ptr.to_deref(), &elem_ty);

    builder.comment("counter := counter + 1");
    builder.add(counter.clone(), counter, ir::Value::LiteralI32(1));

    builder.jmp(start_loop_label);
    builder.label(end_loop_label);

    builder.comment("free the dynamic-allocated buffer - if len > 0");
    builder.eq(
        zero_elements.clone(),
        len_field_ptr.clone().to_deref(),
        ir::Value::LiteralI32(0),
    );
    builder.jmpif(after_free, zero_elements);

    builder.cast(
        arr_mem_ptr.clone(),
        arr_field_ptr.clone().to_deref(),
        ir::Type::U8.ptr(),
    );
    builder.free_mem(arr_mem_ptr);

    builder.append(ir::Instruction::Label(after_free));

    builder.mov(len_field_ptr.to_deref(), ir::Value::LiteralI32(0));
    builder.mov(arr_field_ptr.to_deref(), ir::Value::LiteralNull);

    let releaser_body = builder.finish();

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
        runtime_type.release.expect("dynarray class object must have a release func id allocated"),
        ir::Function::Local(ir::FunctionDef {
            debug_name,
            sig: ir::FunctionSig {
                return_ty: ir::Type::Nothing,
                param_tys: vec![array_struct_ty.clone().ptr()],
            },
            body: releaser_body,
        }),
    );

    let weak_ty = ir::Type::RcWeakPointer(ir::VirtualTypeID::Class(struct_id));
    lib.gen_runtime_type(&weak_ty);
}
