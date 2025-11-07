use crate::instruction_builder::InstructionBuilder;
use crate::FunctionID;
use crate::LocalID;
use crate::Ref;
use crate::Type;
use crate::TypeDefID;
use crate::Value;
use crate::DYNARRAY_PTR_FIELD;

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
            let element_ref = builder.local_temp(element_type.clone().temp_ref());

            for (i, el) in elements.into_iter().enumerate() {
                builder.element(element_ref, arr, Value::LiteralI32(i as i32), element_type.clone(), array_ty.clone());
                builder.mov(element_ref.to_deref(), el);

                // retain each element. we don't do this for static arrays because retaining
                // a static array retains all its elements - for dynamic arrays, retaining
                // the array object itself does not retain the elements
                builder.retain(element_ref.to_deref(), &element_type);
            }
        }
    }
    builder.local_end();
    
    arr.to_ref()
}
