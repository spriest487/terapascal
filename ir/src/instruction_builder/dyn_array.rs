use crate::instruction_builder::InstructionBuilder;
use crate::LocalID;
use crate::Ref;
use crate::Type;
use crate::Value;

pub(super) fn gen_dyn_array_dtor_body<B: InstructionBuilder + ?Sized>(
    builder: &mut B,
    self_param: LocalID,
    elem_ty: &Type,
) {
    let array_type = elem_ty.clone().dyn_array();

    builder.comment("iteration counter");
    let counter = builder.local_temp(Type::I32);
    builder.mov(counter, Value::LiteralI32(0));

    // arr_high := arr.length - 1
    let arr_high = builder.local_temp(Type::I32);
    builder.length(arr_high, self_param, array_type.clone());
    builder.sub(arr_high, arr_high, Value::LiteralI32(1));

    let el_ptr = builder.local_temp(elem_ty.clone().ptr());

    builder.comment("release every element");
    builder.counter_loop(counter, Value::LiteralI32(1), arr_high, |builder| {
        builder.element(el_ptr, self_param, counter, array_type.clone());

        builder.release_deep(el_ptr.to_deref(), &elem_ty);
    });
}

pub(super) fn new_dyn_array(
    builder: &mut impl InstructionBuilder,
    elements: impl IntoIterator<Item=Value>,
    element_type: &Type,
) -> Ref {
    let array_ty = element_type.clone().dyn_array();
    let arr = builder.local_new(array_ty.clone(), None);

    let elements: Vec<_> = elements.into_iter().collect();
    let len = i32::try_from(elements.len()).expect("invalid dynamic array ctor length");

    // allocate the array object itself
    builder.local_begin();
    {
        builder.new_array(arr, element_type.clone(), Value::LiteralI32(len), false);

        // assign elements
        if len > 0 {
            let element_ref = builder.local_temp(element_type.clone().temp_ref());

            for (i, el) in elements.into_iter().enumerate() {
                builder.element(element_ref, arr, Value::LiteralI32(i as i32), array_ty.clone());
                builder.mov(element_ref.to_deref(), el);

                // retain each element. we don't do this for static arrays because retaining
                // a static array retains all its elements - for dynamic arrays, retaining
                // the array object itself does not retain the elements
                builder.retain_deep(element_ref.to_deref(), &element_type);
            }
        }
    }
    builder.local_end();
    
    arr.to_ref()
}
