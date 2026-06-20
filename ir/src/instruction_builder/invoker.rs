use crate::instruction_builder::InstructionBuilder;
use crate::FunctionID;
use crate::GlobalRef;
use crate::FunctionSig;
use crate::LocalID;
use crate::ObjectID;
use crate::Ref;
use crate::Type;
use crate::Value;
use crate::RESULT_REF;
use std::rc::Rc;

/// Build the body of an invoker function that takes the following parameters:
/// - %1: a "self" reference to an object reference,
///     which may be nil for static methods and free functions. passed as a reference
///     so methods operating on value types by reference can replace the boxed self arg
///     with a new box
/// - %2: a dynamic array of object references as arguments
/// - %3: a reference to an object reference where the result value will be stored, if the function
////    returns one.
/// The size of the argument array and types of the arguments will be checked. The result value
/// of the invoker (%0) will be 0 on success and non-zero if the argument array is incorrect.
///
/// The self-argument is treated as argument 0 if non-null, and otherwise behaves the same as the
/// rest of the provided argument list. Keeping it separate means the Invoke method can also
/// take it as a separate parameter and avoid allocating a new array to pass it along with the
/// other arguments. It should be equally valid to ignore it and pass the self-arg as the first
/// element of the array.
///
/// The types of values expected in the argument array are based on the type of the
/// function paramter in the corresponding position:
/// - Object types: expect an object reference of that type (as an Any pointer)
/// - Temp refs: expect a boxed value of the deref type
/// - All other value types: expect a box value of that type
///
/// Boxed arguments will never be modified (as boxes are generally expected to be immutable),
/// but will be replaced in the provided array with new boxes containing their updated values
/// if the parameter they correspond to is a reference.
pub fn gen_invoker_body<B>(
    builder: &mut B,
    func_id: FunctionID,
    func_sig: &FunctionSig,
    self_ref: Ref,
    args_ref: Ref,
    error_out_ref: Ref,
)
where
    B: InstructionBuilder + ?Sized,
{
    let exit_label = builder.next_label();
    let exit_error_label = builder.next_label();

    let params_len = func_sig.param_types.len() as i32;

    let arg_array_type = Type::any().dyn_array();

    builder.comment("exit with error if arg array is null");

    builder.local_begin();
    {
        let args_null = builder.eq_to_val(args_ref.clone(), Value::LiteralNil);
        builder.jmpif(exit_error_label, args_null);
    }
    builder.local_end();

    builder.comment("exit with error if arg array length doesn't match");

    // if self <> nil and self^ <> nil then actual_args_len += 1
    let has_self_arg = builder.local_temp(Type::Bool);

    builder.local_begin();
    {
        let actual_args_len = builder.local_temp(Type::I32);
        builder.length(actual_args_len, args_ref.clone(), arg_array_type.clone());
        
        builder.neq(has_self_arg, self_ref.clone(), Value::LiteralNil);

        builder.if_then(has_self_arg, |builder| {
            builder.neq(has_self_arg, self_ref.clone().to_deref(), Value::LiteralNil);

            builder.if_then(has_self_arg, |builder| {
                builder.add(actual_args_len, actual_args_len, Value::LiteralI32(1));
            })
        });

        let args_len_invalid = builder.neq_to_val(actual_args_len, Value::LiteralI32(params_len));
        builder.jmpif(exit_error_label, args_len_invalid);
    }
    builder.local_end();

    let mut call_args = Vec::with_capacity(func_sig.param_types.len());

    builder.comment("check each arg type");
    let arg_ref = builder.local_temp(Type::any().temp_ref());

    for param_index in 0..func_sig.param_types.len() {
        let param_ty = &func_sig.param_types[param_index];
        let expect_object_id = match param_ty {
            Type::Object(id) | Type::WeakObject(id) => id.clone(),

            Type::TempRef(deref_type) => ObjectID::Box(deref_type.clone()),

            value_type => ObjectID::Box(Rc::new(value_type.clone())),
        };

        // fetch the arg ref
        // if a self-arg was passed, use that for parameter 0 and offset remaining args
        builder.if_then_else(
            has_self_arg.clone(),
            |builder| {
                if param_index == 0 {
                    builder.comment(format!("param {}: self arg", param_index));
                    builder.mov(arg_ref, self_ref.clone());
                } else {
                    let arg_index = param_index as i32 - 1;
                    let index_val = Value::LiteralI32(arg_index);

                    builder.comment(format!("param {}: arg {}", param_index, arg_index));
                    builder.mov(arg_ref, args_ref.clone().element_ref(arg_array_type.clone(), index_val));
                }
            },
            |builder| {
                let index_val = Value::LiteralI32(param_index as i32);

                builder.comment(format!("param {}: arg {}", param_index, param_index));
                builder.mov(arg_ref, args_ref.clone().element_ref(arg_array_type.clone(), index_val));
            },
        );

        builder.comment(format!(
            "exit with error if arg {param_index} is not {}",
            expect_object_id
                .to_object_type()
                .to_pretty_string(builder.metadata())
        ));

        builder.local_begin();
        {
            let invalid_arg = builder.local_temp(Type::Bool);

            builder.is_type(invalid_arg, arg_ref.to_deref(), Type::any(), expect_object_id.to_object_type());
            builder.not(invalid_arg, invalid_arg);
            builder.jmpif(exit_error_label, invalid_arg);
        }
        builder.local_end();

        match param_ty {
            // object types: downcast and pass the arg directly
            Type::Object(..) | Type::WeakObject(..) => {
                builder.local_begin();
                {
                    builder.comment("object param: copy the argument");
                    let call_arg = builder.local_temp(param_ty.clone());
                    builder.cast(call_arg, arg_ref.to_deref(), param_ty.clone());

                    call_args.push(call_arg.value());
                }
                builder.local_end();
            }

            Type::TempRef(deref_type) => {
                // unbox...
                let value_type = deref_type.as_ref();

                let new_val_ref = builder.local_temp(value_type.clone().temp_ref());

                builder.comment("ref param: clone the boxed argument into a new box");
                builder.local_begin();
                {
                    gen_ref_arg_boxed_value(builder, new_val_ref, arg_ref, value_type);
                }
                builder.local_end();

                // pass the ref to the new box's element (func expects a ref)
                call_args.push(new_val_ref.value());
            },

            value_type => {
                // unbox the arg and pass that
                let unboxed_val = builder.local_temp(value_type.clone());

                builder.comment("value param: unbox the argument");
                builder.local_begin();
                {
                    let box_type = value_type.clone().boxed();
                    let arg_box = builder.local_temp(box_type.clone());

                    builder.cast(arg_box, arg_ref.to_deref(), box_type.clone());
                    builder.mov(unboxed_val, arg_box.to_ref().element_ref(box_type, Value::I32_0).to_deref());
                }
                builder.local_end();

                call_args.push(unboxed_val.value());
            },
        }
    }

    if func_sig.result_type == Type::Nothing {
        // no result: set result ref to nil
        builder.call(GlobalRef::func(func_id, []), call_args, None);
        builder.mov(RESULT_REF, Value::LiteralNil);
    } else {
        let call_result = builder.local_temp(func_sig.result_type.clone());
        builder.call(GlobalRef::func(func_id, []), call_args, Some(call_result.to_ref()));

        gen_box_result(builder, &func_sig.result_type, call_result.value());
    }

    builder.mov(error_out_ref.clone(), Value::I32_0);
    builder.jmp(exit_label);

    builder.label(exit_error_label);
    builder.mov(error_out_ref.clone(), Value::I32_1);
    builder.mov(RESULT_REF, Value::LiteralNil);

    builder.label(exit_label);
}

// for ref args: 
// the arg value is expected to be a box of the value. we clone this box into a new box of the same
// type (since the function may modify the value, it can't be allowed to modify the original box),
// and return a ref (the actual arg) to the new box's value
fn gen_ref_arg_boxed_value<B>(
    builder: &mut B,
    ref_out: LocalID,
    boxed_arg: LocalID,
    value_type: &Type,
)
where
    B: InstructionBuilder + ?Sized,
{
    let box_type = value_type.clone().boxed();

    let arg_box = builder.local_temp(box_type.clone());
    builder.cast(arg_box, boxed_arg.to_deref(), box_type.clone());

    let boxed_value = builder.element_to_val(
        arg_box,
        Value::I32_0,
        value_type.clone(),
        box_type.clone(),
    );

    let new_box = builder.local_temp(box_type.clone());

    builder.new_box(new_box, value_type.clone(), false);

    builder.mov(ref_out, new_box.to_ref().element_ref(box_type.clone(), Value::I32_0));
    builder.mov(ref_out.to_deref(), boxed_value);

    builder.release(boxed_arg.to_deref(), box_type, Ref::Discard);
    builder.cast(boxed_arg.to_deref(), new_box, Type::any());
}

fn gen_box_result<B>(builder: &mut B, return_type: &Type, call_result: Value)
where
    B: InstructionBuilder + ?Sized,
{
    if return_type.is_object() {
        // result is an object: replace the result ref
        builder.cast(RESULT_REF, call_result, Type::any());
        return;
    }

    // box the result
    let result_box_type = return_type.clone().boxed();

    let result_box = builder.local_temp(result_box_type.clone());
    builder.new_box(result_box, return_type.clone(), false);

    let result_box_element_ref = result_box.to_ref().element_ref(result_box_type, Value::I32_0);
    builder.mov(result_box_element_ref.to_deref(), call_result);

    builder.cast(RESULT_REF, result_box, Type::any());
}
