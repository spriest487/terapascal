use crate::instruction_builder::InstructionBuilder;
use crate::Ref;
use crate::Type;
use crate::Value;
use crate::ArgID;

pub(super) fn gen_dyn_array_dtor_body<B: InstructionBuilder + ?Sized>(
    builder: &mut B,
    self_param: ArgID,
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

    builder.comment("release every element");
    builder.counter_loop(counter, Value::LiteralI32(1), arr_high, |builder| {
        let element_ref = self_param.to_ref().element_ref(array_type.clone(), counter);

        builder.release(element_ref.to_deref(), elem_ty.clone(), Ref::Discard);
    });
}

pub(super) fn new_array_from(
    builder: &mut impl InstructionBuilder,
    elements: impl IntoIterator<Item=Value>,
    element_type: &Type,
) -> Ref {
    let array_ty = element_type.clone().dyn_array();
    let arr = builder.local_var(array_ty.clone(), None);

    let elements: Vec<_> = elements.into_iter().collect();
    let len = i32::try_from(elements.len()).expect("invalid dynamic array ctor length");

    // allocate the array object itself
    builder.local_begin();
    {
        builder.new_array(arr, element_type.clone(), Value::LiteralI32(len), false);

        // assign elements
        if len > 0 {
            for (i, el) in elements.into_iter().enumerate() {
                let index_val = Value::LiteralI32(i as i32);
                let element_ref = arr.to_ref().element_ref(array_ty.clone(), index_val);
                builder.mov(element_ref.to_deref(), el);

                // retain each element. we don't do this for static arrays because retaining
                // a static array retains all its elements - for dynamic arrays, retaining
                // the array object itself does not retain the elements
                builder.retain(element_ref.to_deref(), element_type.clone());
            }
        }
    }
    builder.local_end();
    
    arr.to_ref()
}
