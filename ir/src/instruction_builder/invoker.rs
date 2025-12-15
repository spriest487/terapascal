use crate::FunctionID;
use crate::FunctionSig;
use crate::LocalID;
use crate::ObjectID;
use crate::RETURN_REF;
use crate::Ref;
use crate::Type;
use crate::Value;
use crate::instruction_builder::InstructionBuilder;
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
pub fn gen_invoker_body<B>(builder: &mut B, func_id: FunctionID, func_sig: &FunctionSig)
where
    B: InstructionBuilder + ?Sized,
{
    let self_arg_ref = LocalID(1);
    let args_arg = LocalID(2);
    let result_code_ref_arg = LocalID(3);

    let exit_label = builder.next_label();
    let exit_error_label = builder.next_label();

    let params_len = func_sig.param_tys.len() as i32;

    let arg_array_type = Type::any().dyn_array();

    builder.comment("exit with error if arg array is null");
    
    builder.local_begin();
    {
        let args_null = builder.eq_to_val(args_arg, Value::LiteralNull);
        builder.jmpif(exit_error_label, args_null);
    }
    builder.local_end();

    builder.comment("exit with error if arg array length doesn't match");

    // if self <> nil and self^ <> nil then actual_args_len += 1
    let has_self_arg = builder.local_temp(Type::Bool);

    builder.local_begin();
    {
        let actual_args_len = builder.local_temp(Type::I32);
        builder.length(actual_args_len, args_arg, arg_array_type.clone());
        
        builder.neq(has_self_arg, self_arg_ref, Value::LiteralNull);

        builder.if_then(has_self_arg, |builder| {
            builder.neq(has_self_arg, self_arg_ref.to_deref(), Value::LiteralNull);

            builder.if_then(has_self_arg, |builder| {
                builder.add(actual_args_len, actual_args_len, Value::LiteralI32(1));
            })
        });

        let args_len_invalid = builder.neq_to_val(actual_args_len, Value::LiteralI32(params_len));
        builder.jmpif(exit_error_label, args_len_invalid);
    }
    builder.local_end();

    let mut call_args = Vec::with_capacity(func_sig.param_tys.len());

    builder.comment("check each arg type");
    let arg_ref = builder.local_temp(Type::any().temp_ref());

    for param_index in 0..func_sig.param_tys.len() {
        builder.local_begin();
        {
            let param_ty = &func_sig.param_tys[param_index];
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
                        builder.mov(arg_ref, self_arg_ref);
                    } else {
                        let arg_index = param_index as i32 - 1;
                        let index_val = Value::LiteralI32(arg_index);

                        builder.comment(format!("param {}: arg {}", param_index, arg_index));
                        builder.element(arg_ref, args_arg, index_val, arg_array_type.clone());
                    }
                },
                |builder| {
                    builder.comment(format!("param {}: arg {}", param_index, param_index));
                    builder.element(
                        arg_ref,
                        args_arg,
                        Value::LiteralI32(param_index as i32),
                        arg_array_type.clone(),
                    );
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
                let arg_is_valid = builder.local_temp(Type::Bool);
                let arg_is_invalid = builder.local_temp(Type::Bool);

                builder.class_is(arg_is_valid, arg_ref.to_deref(), expect_object_id);
                builder.not(arg_is_invalid, arg_is_valid);
                builder.jmpif(exit_error_label, arg_is_invalid);
            }
            builder.local_end();

            match param_ty {
                // object types: downcast and pass the arg directly
                Type::Object(..) | Type::WeakObject(..) => {
                    builder.comment("object param: copy the argument");
                    let call_arg = builder.local_temp(param_ty.clone());
                    builder.cast(call_arg, arg_ref.to_deref(), param_ty.clone());

                    call_args.push(call_arg.value());
                },

                Type::TempRef(deref_type) => {
                    // unbox...
                    let value_type = deref_type.as_ref();
                    let box_type = value_type.clone().boxed();

                    let new_val_ref = builder.local_temp(value_type.clone().temp_ref());

                    builder.local_begin();
                    {
                        let arg_box = builder.local_temp(box_type.clone());
                        builder.cast(arg_box, arg_ref.to_deref(), box_type.clone());

                        let arg_val = builder.element_to_val(
                            arg_box,
                            Value::I32_0,
                            value_type.clone(),
                            box_type.clone(),
                        );

                        builder.comment("ref param: clone the boxed argument into a new box");
                        let new_box = builder.local_temp(box_type.clone());

                        builder.new_box(new_box, value_type.clone(), false);
                        builder.element(new_val_ref, new_box, Value::I32_0, box_type.clone());
                        builder.mov(new_val_ref.to_deref(), arg_val);

                        builder.release(arg_ref.to_deref(), false, Ref::Discard);
                        builder.cast(arg_ref.to_deref(), new_box, Type::any());
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
                        builder.element_val(unboxed_val, arg_box, Value::I32_0, value_type.clone(), box_type);
                    }
                    builder.local_end();

                    call_args.push(unboxed_val.value());
                },
            }
        }
        builder.local_end();
    }

    if func_sig.return_ty == Type::Nothing {
        // no result: set result ref to nil
        builder.call(func_id, call_args, None);
        builder.mov(RETURN_REF, Value::LiteralNull);
    } else {
        let call_result = builder.local_temp(func_sig.return_ty.clone());
        builder.call(func_id, call_args, Some(call_result.to_ref()));

        if func_sig.return_ty.is_object() {
            // result is an object: replace the result ref
            builder.cast(RETURN_REF, call_result, Type::any());
        } else {
            // box the result
            let result_box_type = func_sig.return_ty.clone().boxed();

            let result_box = builder.local_temp(result_box_type.clone());
            let result_box_value_ref = builder.local_temp(func_sig.return_ty.clone().temp_ref());

            builder.new_box(result_box, func_sig.return_ty.clone(), false);
            builder.element(
                result_box_value_ref,
                result_box,
                Value::LiteralI32(0),
                result_box_type,
            );
            builder.mov(result_box_value_ref.to_deref(), call_result);

            builder.cast(RETURN_REF, result_box, Type::any());
        }
    }

    builder.mov(result_code_ref_arg.to_deref(), Value::I32_0);
    builder.jmp(exit_label);

    builder.label(exit_error_label);
    builder.mov(result_code_ref_arg.to_deref(), Value::I32_1);
    builder.mov(RETURN_REF, Value::LiteralNull);

    builder.label(exit_label);
}
