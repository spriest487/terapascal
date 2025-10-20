use crate::instruction_builder::InstructionBuilder;
use crate::FunctionID;
use crate::LocalID;
use crate::Ref;
use crate::Type;
use crate::TypeDefID;
use crate::Value;
use crate::VirtualTypeID;
use crate::DYNARRAY_LEN_FIELD;
use crate::DYNARRAY_PTR_FIELD;
use crate::RETURN_REF;

/// generate a function body for a dyn array alloc function, assumed to have the following sig:
/// %0: array object pointer (any)
/// %1: new length (i32)
/// %2: optional source array to copy elements from (any, element type must match array in %0)
/// %3: default element pointer - if length is greater than the source array's length 
///     (or greater than 0, if the source is array null), must point to a value of the same type as
///     the array element, to be copied into each new array item
pub(super) fn gen_dyn_array_alloc_body(
    builder: &mut impl InstructionBuilder,
    elem_ty: &Type,
    struct_id: TypeDefID,
    get_mem_id: FunctionID,
) {
    let array_ref_ty = Type::RcPointer(VirtualTypeID::Class(struct_id));
    let el_ptr_ty = elem_ty.clone().ptr();

    let len_arg = LocalID(1);
    let default_val_arg = LocalID(3);
    
    // var default_el_ptr := L3 (default val param)
    let default_el_ptr = builder.local_temp(el_ptr_ty.clone());
    builder.cast(default_el_ptr, default_val_arg, el_ptr_ty.clone());
    
    // if the default value arg is not provided, create a default/zero-initialized value
    // var default_el: T;
    // if default_el_ptr = nil then 
    //   default_el := default(T);
    //   default_el_ptr := @default_el
    let has_default_label = builder.next_label();
    let default_el = builder.local_temp(elem_ty.clone());
    
    let has_default_val = builder.neq_to_val(default_el_ptr, Value::LiteralNull);
    builder.jmpif(has_default_label, has_default_val);
    builder.local_begin();
    {
        builder.gen_default_init(default_el, elem_ty);
        builder.addr_of(default_el_ptr, default_el);
    }
    builder.local_end();
    
    builder.label(has_default_label);

    builder.comment("cast the array params to this array type");
    let arr = builder.local_temp(array_ref_ty.clone());
    let src_arr = builder.local_temp(array_ref_ty.clone());
    builder.cast(
        arr.clone(),
        Ref::Local(LocalID(0)),
        array_ref_ty.clone(),
    );
    builder.cast(
        src_arr.clone(),
        Ref::Local(LocalID(2)),
        array_ref_ty.clone(),
    );

    builder.comment("el_len := sizeof(elem_ty)");
    let el_len = builder.local_temp(Type::I32);
    builder.size_of(el_len.clone(), elem_ty.clone());

    builder.comment("data_len := el_len * len");
    let data_len = builder.local_temp(Type::I32);
    builder.mul(data_len.clone(), el_len.clone(), len_arg);

    builder.comment("data = GetMem(data_len) as ^elem_ty");
    let data_mem = builder.local_temp(Type::U8.ptr());
    builder.call(get_mem_id, [data_len.value()], Some(data_mem.to_ref()));

    let data = builder.local_temp(el_ptr_ty.clone());
    builder.cast(data, data_mem, el_ptr_ty.clone());

    builder.comment("iteration counter for initializing elements");
    let counter = builder.local_temp(Type::I32);
    builder.mov(counter, Value::LiteralI32(0));

    builder.comment("loop break flag we use in a couple of places later");
    let done = builder.local_temp(Type::Bool);

    builder.comment("copy elements from copied array");

    let skip_copy_label = builder.next_label();
    builder.comment("skip copying from source if copy_from is null");
    let src_is_null = builder.eq_to_val(src_arr.clone(), Value::LiteralNull);
    builder.jmpif(skip_copy_label, src_is_null);

    builder.comment("copy_len := copy_from->length");
    let src_len = builder.local_temp(Type::I32);
    builder.field_val(
        src_len,
        src_arr,
        array_ref_ty.clone(),
        DYNARRAY_LEN_FIELD,
        Type::I32,
    );

    builder.local_begin();
    {
        let copy_count = builder.local_temp(Type::I32);
        let copy_count_ok = builder.local_temp(Type::Bool);

        builder.comment("copy_count := src_len");
        builder.mov(copy_count, src_len.clone());

        builder.comment(
            "if there are more elements in the source than we want, copy `len` elements instead",
        );
        let copy_count_ok_label = builder.next_label();

        builder.comment("copy_count_ok := copy_count <= len");
        builder.lte(copy_count_ok, copy_count.clone(), len_arg);

        builder.comment("if not copy_count_ok then copy_count := len");
        builder.jmpif(copy_count_ok_label, copy_count_ok);
        builder.mov(copy_count, len_arg);
        builder.label(copy_count_ok_label);

        builder.comment(
            "for `copy_count` iterations, copy the value at copy_src[counter] to copy_dst[counter]",
        );
        let copy_loop_label = builder.next_label();
        let copy_break_label = builder.next_label();

        builder.label(copy_loop_label);

        builder.comment("done := counter = copy_count");
        builder.comment("if done then break");
        builder.eq(done, counter, copy_count);
        builder.jmpif(copy_break_label, done);

        builder.local_begin();
        {
            let copy_dst = builder.local_temp(el_ptr_ty.clone());
            let copy_src = builder.local_temp(el_ptr_ty.clone());

            builder.comment("copy_dst := data + counter");
            builder.add(copy_dst, data, counter);

            builder.comment("copy_src := src_arr->ptr + counter");
            builder.field_val(
                copy_src,
                src_arr,
                array_ref_ty.clone(),
                DYNARRAY_PTR_FIELD,
                el_ptr_ty.clone(),
            );
            builder.add(copy_src, copy_src, counter);

            builder.comment("copy_dst^ := copy_src^");
            builder.mov(copy_dst.to_deref(), copy_src.to_deref());

            builder.retain(copy_dst.to_deref(), elem_ty);
        }
        builder.local_end();

        builder.comment("counter += 1");
        builder.add(counter.clone(), counter.clone(), Value::LiteralI32(1));

        builder.jmp(copy_loop_label);
        builder.label(copy_break_label);
    }
    builder.local_end();

    builder.label(skip_copy_label);

    builder.comment("while counter < len, default init next element");
    let init_break_label = builder.next_label();
    let init_loop_label = builder.next_label();

    builder.label(init_loop_label);

    builder.comment("done := counter = len");
    builder.comment("if done then break");
    builder.eq(done.clone(), counter.clone(), len_arg);
    builder.jmpif(init_break_label, done);

    builder.local_begin();
    {
        builder.comment("data[counter] := default_val_ptr^");
        let el_ptr = builder.local_temp(el_ptr_ty.clone());
        builder.add(el_ptr, data, counter);
        builder.mov(el_ptr.to_deref(), default_el_ptr.to_deref());

        builder.retain(el_ptr.to_deref(), elem_ty);
    }
    builder.local_end();

    builder.comment("counter += 1");
    builder.add(counter.clone(), counter.clone(), Value::LiteralI32(1));
    builder.jmp(init_loop_label);

    builder.label(init_break_label);

    builder.assign_field(
        arr.clone(),
        array_ref_ty.clone(),
        DYNARRAY_LEN_FIELD,
        Type::I32,
        len_arg,
    );
    builder.assign_field(arr, array_ref_ty, DYNARRAY_PTR_FIELD, el_ptr_ty, data);
}

pub(super) fn gen_dyn_array_length_body(
    builder: &mut impl InstructionBuilder,
    array_class_id: TypeDefID,
) {
    builder.comment("cast pointer down to this array type");
    let arr = builder.local_temp(array_class_id.to_class_ptr_type());
    
    builder.cast(
        arr.clone(),
        Ref::Local(LocalID(1)),
        array_class_id.to_class_ptr_type(),
    );

    builder.comment("evaluate length field into return ref");
    builder.field_val(
        RETURN_REF,
        arr,
        array_class_id.to_class_ptr_type(),
        DYNARRAY_LEN_FIELD,
        Type::I32,
    );
}

pub(super) fn gen_dyn_array_dtor_body(
    builder: &mut impl InstructionBuilder,
    elem_ty: &Type,
    array_class_id: TypeDefID,
    free_mem_id: FunctionID,
) {
    let class_type = array_class_id.to_class_ptr_type();

    let self_arg = LocalID(0);
    builder.retain(self_arg, &class_type);

    builder.comment("iteration counter");
    let counter = builder.local_temp(Type::I32);
    builder.mov(counter, Value::LiteralI32(0));

    // arr_high := arr.length - 1
    let arr_high = builder.local_temp(Type::I32);
    builder.length(arr_high, self_arg, class_type.clone());
    builder.sub(arr_high, arr_high, Value::LiteralI32(1));

    let el_ptr = builder.local_temp(elem_ty.clone().ptr());

    builder.comment("release every element");
    builder.counter_loop(counter, Value::LiteralI32(1), arr_high, |builder| {
        builder.element(el_ptr, self_arg, counter, elem_ty.clone(), class_type.clone());

        builder.release(el_ptr.to_deref(), &elem_ty);
    });

    builder.comment("pointer to the pointer field of the dynarray object");
    let arr_ptr = builder.field_to_val(
        self_arg,
        class_type,
        DYNARRAY_PTR_FIELD,
        elem_ty.clone().ptr(),
    );

    builder.comment("u8 pointer var, to cast the array memory pointer before calling FreeMem");
    let arr_mem_ptr = builder.local_temp(Type::U8.ptr());

    // FreeMem(instance.array as Byte^)
    builder.cast(arr_mem_ptr, arr_ptr, Type::U8.ptr());
    builder.call(free_mem_id, [arr_mem_ptr.value()], None);
}

pub(super) fn new_dyn_array(
    builder: &mut impl InstructionBuilder,
    array_class_id: TypeDefID,
    elements: impl IntoIterator<Item=Value>,
    element_type: &Type,
) -> Ref {
    let array_ty = array_class_id.to_class_ptr_type();
    let arr = builder.local_new(array_ty.clone(), None);

    let elements: Vec<_> = elements.into_iter().collect();
    let len = i32::try_from(elements.len()).expect("invalid dynamic array ctor length");

    // allocate the array object itself
    builder.local_begin();
    {
        builder.rc_new_array(arr, element_type.clone(), Value::LiteralI32(len), false);

        // assign elements
        if len > 0 {
            let el_ptr = builder.local_temp(element_type.clone().ptr());

            for (i, el) in elements.into_iter().enumerate() {
                builder.element(el_ptr, arr, Value::LiteralI32(i as i32), element_type.clone(), array_ty.clone());
                builder.mov(el_ptr.to_deref(), el);

                // retain each element. we don't do this for static arrays because retaining
                // a static array retains all its elements - for dynamic arrays, retaining
                // the array object itself does not retain the elements
                builder.retain(el_ptr.to_deref(), &element_type);
            }
        }
    }
    builder.local_end();
    
    arr.to_ref()
}
