use crate::func::BuiltinFn;
use crate::DynValue;
use crate::ExecError;
use crate::ExecResult;
use crate::Interpreter;
use crate::Pointer;
use ir_lang::*;
use rand::Rng;
use std::env::consts::OS;
use std::fmt;
use std::io;
use std::io::BufRead;
use std::io::Write;

fn primitive_to_str<T, UnwrapFn>(state: &mut Interpreter, unwrap_fn: UnwrapFn) -> ExecResult<()>
where
    T: fmt::Display,
    UnwrapFn: FnOnce(&DynValue) -> Option<T>,
{
    let arg_0 = Ref::Local(LocalID(1));

    let arg_0_dyn = state.load(&arg_0)?;
    let value = unwrap_fn(&arg_0_dyn).ok_or_else(|| {
        ExecError::illegal_state("primitive_to_str argument is not the correct type".to_string())
    })?;

    let string = state.create_string(&value.to_string(), false)?;
    state.store(&RETURN_REF, string)?;

    Ok(())
}

/// %1: I8 -> %0: String
pub(super) fn i8_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i8)
}

/// %1: U8 -> %0: String
pub(super) fn u8_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u8)
}

/// %1: I16 -> %0: String
pub(super) fn i16_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i16)
}

/// %1: U16 -> %0: String
pub(super) fn u16_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u16)
}

/// %1: I32 -> %0: String
pub(super) fn i32_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i32)
}

/// %1: U32 -> %0: String
pub(super) fn u32_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u32)
}

/// %1: I64 -> %0: String
pub(super) fn i64_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_i64)
}

/// %1: U64 -> %0: String
pub(super) fn u64_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_u64)
}

/// %1: ISize -> %0: String
pub(super) fn isize_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_isize)
}

/// %1: USize -> %0: String
pub(super) fn usize_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_usize)
}

/// %1: Pointer -> %0: String
pub(super) fn pointer_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, |val| {
        val.as_pointer().cloned()
    })
}

/// %1: F32 -> %0: String
pub(super) fn real_to_str(state: &mut Interpreter) -> ExecResult<()> {
    primitive_to_str(state, DynValue::as_f32)
}

/// %1: String -> %0: I32
pub(super) fn str_to_int(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(1));

    let string = state.read_string(&arg_0)?;
    let int: i32 = string.parse().map_err(|_| ExecError::Raised {
        msg: format!("could not convert `{}` to Integer", string),
    })?;

    state.store(&RETURN_REF, DynValue::I32(int))?;

    Ok(())
}

/// %0: String -> Nothing
pub(super) fn write(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(0));
    let string = state.read_string(&arg_0)?;

    _ = io::stdout().lock().write_all(string.as_bytes()).unwrap();

    Ok(())
}

/// %0: String -> Nothing
pub(super) fn write_ln(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(0));
    let string = state.read_string(&arg_0)?;

    let mut stdout = io::stdout().lock();

    _ = stdout.write_all(string.as_bytes());
    _ = stdout.write_all(match OS {
       "windows" => "\r\n".as_bytes(),
        _ => "\n".as_bytes(),
    });
    _ = stdout.flush();

    Ok(())
}

/// %0: Nothing -> String
pub(super) fn read_ln(state: &mut Interpreter) -> ExecResult<()> {
    let stdin = io::stdin();
    let mut line = String::new();

    if let Err(_) = stdin.lock().read_line(&mut line) {
        line = String::new();
    }

    // remove the newline
    if line.len() > 0 {
        line.remove(line.len() - 1);
    }

    let result_str = state.create_string(&line, false)?;

    state.store(&RETURN_REF, result_str)?;

    Ok(())
}

/// %1: Integer -> %0: ^Byte
pub(super) fn get_mem(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(1));

    let len = state
        .load(&arg_0)?
        .as_i32()
        .ok_or_else(|| ExecError::illegal_state("GetMem expected I32 argument"))?;

    let mem_ptr = if len != 0 {
        state.dynalloc(&Type::U8, len as usize)?
    } else {
        Pointer::nil(Type::U8)
    };

    state.store(&RETURN_REF, DynValue::Pointer(mem_ptr))?;

    Ok(())
}

/// %0: ^Byte -> Nothing
pub(super) fn free_mem(state: &mut Interpreter) -> ExecResult<()> {
    let arg_0 = Ref::Local(LocalID(0));

    let ptr_val = state.load(&arg_0)?;

    let ptr = ptr_val
        .as_pointer()
        .ok_or_else(|| ExecError::illegal_state("FreeMem expected heap pointer argument"))?;
    
    if !ptr.is_null() {
        state.dynfree(ptr)?;
    }

    Ok(())
}

fn get_ref_type_id(state: &Interpreter, at: &Ref) -> ExecResult<TypeDefID> {
    let ptr = state.load(at)?
        .as_pointer()
        .cloned()
        .ok_or_else(|| ExecError::illegal_state("get_ref_type_id: argument val must be pointer"))?;

    match state.load_indirect(&ptr)? {
        DynValue::Structure(struct_val) => Ok(struct_val.type_id),

        _ => Err(ExecError::illegal_state(
            "value pointed to by dynarray pointer is not a dynarray",
        )),
    }
}

/// %1: <any dyn array ref> -> %0: Integer
pub(super) fn array_length(state: &mut Interpreter) -> ExecResult<()> {
    let array_arg_ref = Ref::Local(LocalID(1));
    let array_type_id = get_ref_type_id(state, &array_arg_ref)?;

    let len_func = state.metadata.dyn_array_element_ty(array_type_id)
        .and_then(|elem_ty| {
            let rtti = state.metadata.get_dynarray_runtime_type(elem_ty)?;
            Some(rtti.length)
        })
        .ok_or_else(|| {
            let msg = format!("missing dynarray length generated function for type ID {}", array_type_id);
            ExecError::illegal_state(msg)
        })?;

    let arr_arg = state.evaluate(&Value::Ref(array_arg_ref))?;
    state.call_store(len_func, &[arr_arg], Some(&RETURN_REF))?;

    Ok(())
}

/// %1: <any dyn array ref>; %2: Integer; %3: ^Element; -> %0: <dyn array ref of same type>
pub(super) fn set_length(state: &mut Interpreter) -> ExecResult<()> {
    let array_arg_ref = Ref::Local(LocalID(1));
    let new_len_arg = Ref::Local(LocalID(2));
    let default_val_arg = Ref::Local(LocalID(3));

    let array_type_id = get_ref_type_id(state, &array_arg_ref)?;
    let new_len = state.evaluate(&Value::Ref(new_len_arg))?;
    let default_val_ptr = state.evaluate(&Value::Ref(default_val_arg))?;

    let alloc_func = state.metadata.dyn_array_element_ty(array_type_id)
        .and_then(|elem_ty| {
            let rtti = state.metadata.get_dynarray_runtime_type(elem_ty)?;
            Some(rtti.alloc)
        })
        .ok_or_else(|| {
            let msg = format!("missing dynarray length generated function for type ID {}", array_type_id);
            ExecError::illegal_state(msg)
        })?;

    let new_arr_struct = state.init_struct(array_type_id)?;

    let old_arr = state.evaluate(&Value::Ref(array_arg_ref))?;
    let new_arr = state.rc_alloc(new_arr_struct, false)?;
    let new_arr_val = DynValue::Pointer(new_arr);

    state.call_store(alloc_func, &[new_arr_val.clone(), new_len, old_arr, default_val_ptr], None)?;

    state.store(&RETURN_REF, new_arr_val)?;

    Ok(())
}

fn invoke_method(state: &mut Interpreter) -> ExecResult<()> {
    let method_ptr = load_pointer(state, &Ref::Local(LocalID(0)))?;
    let instance_ptr = load_pointer(state, &Ref::Local(LocalID(1)))?;
    let arg_array_ptr = load_pointer(state, &Ref::Local(LocalID(2)))?;
    let result_ptr_arg = load_pointer(state, &Ref::Local(LocalID(3)))?;

    let (method_info_val,_) = state.load_rc_struct_ptr(&method_ptr)?;
    
    let method_name_ptr = method_info_val[METHODINFO_NAME_FIELD]
        .as_pointer()
        .ok_or_else(|| {
            let msg = format!("bad type: expected string pointer at field {METHODINFO_NAME_FIELD} of method info");
            ExecError::illegal_state(msg)
        })?;

    // TODO: MethodInfo should be know its own method index/ID for overloading?
    let method_name = state.read_string_indirect(method_name_ptr)?;

    let type_info_ptr = method_info_val[METHODINFO_OWNER_FIELD]
        .as_pointer()
        .ok_or_else(|| {
            let msg = format!("bad type: expected owner pointer at field {METHODINFO_OWNER_FIELD} of method info");
            ExecError::illegal_state(msg)
        })?;

    let (type_info_val, _) = state.load_rc_struct_ptr(&type_info_ptr)?;
    let type_name_ptr = type_info_val[TYPEINFO_NAME_FIELD]
        .as_pointer()
        .ok_or_else(|| {
            let msg = format!("bad type: expected string pointer at field {TYPEINFO_NAME_FIELD} of type info");
            ExecError::illegal_state(msg)
        })?;

    let type_name = state.read_string_indirect(type_name_ptr)?;
    
    let (ty, runtime_type, method_index) = state.metadata
        .find_runtime_method(&type_name, &method_name)
        .ok_or_else(|| {
            let msg = format!("attempting to invoke method {}.{} which does not exist in metadata", type_name, method_name);
            ExecError::illegal_state(msg)
        })?;
    
    let method = runtime_type.methods[method_index].clone();

    let mut call_arg_vals = Vec::new();
    if !instance_ptr.is_null() { 
        let instance_val = state.load_indirect(&instance_ptr.reinterpret(ty.clone()))?;
        call_arg_vals.push(instance_val);
    }
    
    let arg_array = state.read_dynarray(&arg_array_ptr)?;
    let total_arg_count = call_arg_vals.len() + arg_array.len();

    if total_arg_count != method.params.len() {
        let msg = format!(
            "method invoke arg array for {}.{} did not match expected size (got {}, expected {})",
            type_name,
            method_name,
            total_arg_count,
            method.params.len(),
        );
        return Err(ExecError::illegal_state(msg));
    }
    
    let mut param_index = call_arg_vals.len();

    for arg in arg_array {
        let arg_ptr = arg
            .as_pointer()
            .ok_or_else(|| ExecError::illegal_state("method_invoke arg array may only contain pointers"))?
            .reinterpret(method.params[param_index].clone());

        let arg_val = state.load_indirect(&arg_ptr)?;
        
        call_arg_vals.push(arg_val);
        param_index += 1;
    }

    match method.function {
        Some(func_id) => {
            let result_val = state.call(func_id, &call_arg_vals)?;
            if !result_ptr_arg.is_null() {
                let Some(result_val) = result_val else {
                    return Err(ExecError::illegal_state("result pointer was provided but function did not return a value"));
                };

                let result_ptr = result_ptr_arg.reinterpret(method.result_ty.clone());
                state.store_indirect(&result_ptr, result_val)?;
            }

            Ok(())
        }
        
        None => {
            Err(ExecError::illegal_state("InvokeMethod called for abstract method"))
        }
    }
}

fn find_type_info(state: &mut Interpreter) -> ExecResult<()> {
    let name_arg = state.read_string(&Ref::Local(LocalID(1)))?;
    
    let result = match state.typeinfo_by_name.get(&name_arg).cloned() {
        Some(typeinfo_global) => {
            state.load(&Ref::Global(typeinfo_global))?
        }
        
        None => {
            DynValue::nil(TYPEINFO_TYPE)
        }
    };
    
    state.store(&RETURN_REF, result)?;
    
    Ok(())
}

fn get_type_info_count(state: &mut Interpreter) -> ExecResult<()> {
    let count = i32::try_from(state.typeinfo_by_name.len())
        .unwrap_or(i32::MAX);

    state.store(&RETURN_REF, DynValue::I32(count))
}

fn get_type_info_by_index(state: &mut Interpreter) -> ExecResult<()> {
    let index_param_local = Ref::Local(LocalID(1));

    let index = state.load(&index_param_local)?
        .as_i32()
        .ok_or_else(|| {
            ExecError::illegal_state("parameter to get_type_info must be i32")
        })?;

    let type_info_ref = usize::try_from(index)
        .ok()
        .and_then(|i| state.typeinfo_refs.get(i).cloned())
        .ok_or_else(|| ExecError::illegal_state(format!("illegal TypeInfo index: {index}")))?;
    
    let type_info_ptr = state.load(&Ref::Global(type_info_ref))?;
    state.store(&RETURN_REF, type_info_ptr)?;
    
    Ok(())
}

fn get_object_type_info(state: &mut Interpreter) -> ExecResult<()> {
    let obj_param = Ref::Local(LocalID(1));

    let type_id = state
        .load(&obj_param.to_deref())?
        .as_any_struct()
        .ok_or_else(|| {
            ExecError::illegal_state("object pointer must refer to a struct value")
        })?
        .type_id;
    
    let type_info_ref = state
        .get_class_runtime_type_ref(type_id)
        .ok_or_else(|| {
            ExecError::illegal_state(format!("missing runtime type info for class struct {type_id}"))
        })?;
    
    let type_info_ptr = state.load(&Ref::Global(type_info_ref))?;
    state.store(&RETURN_REF, type_info_ptr)?;
    
    Ok(())
}

fn load_pointer(state: &mut Interpreter, at: &Ref) -> ExecResult<Pointer> {
    state
        .load(at)?
        .as_pointer()
        .cloned()
        .ok_or_else(|| {
            let msg = format!("bad type: expected pointer at {at}");
            ExecError::illegal_state(msg)
        })
}

fn load_integer(state: &mut Interpreter, at: &Ref) -> ExecResult<i32> {
    state.load(at)?
        .as_i32()
        .ok_or_else(|| {
            let msg = format!("bad type: expected i32 at {}", at);
            ExecError::IllegalState { msg }
        })
}

fn load_single(state: &mut Interpreter, at: &Ref) -> ExecResult<f32> {
    state.load(at)?
        .as_f32()
        .ok_or_else(|| {
            let msg = format!("bad type: expected f32 at {}", at);
            ExecError::IllegalState { msg }
        })
}

pub(super) fn random_integer(state: &mut Interpreter) -> ExecResult<()> {
    let from = load_integer(state, &Ref::Local(LocalID(1)))?;
    let to = load_integer(state, &Ref::Local(LocalID(2)))?;

    let range = from..to;
    let val = if range.is_empty() {
        from
    } else {
        rand::rng().random_range(from..to)
    };

    state.store(&RETURN_REF, DynValue::I32(val))
}

pub(super) fn random_single(state: &mut Interpreter) -> ExecResult<()> {
    let from = load_single(state, &Ref::Local(LocalID(1)))?;
    let to = load_single(state, &Ref::Local(LocalID(2)))?;

    let range = from..to;
    let val = if range.is_empty() {
        from
    } else {
        rand::rng().random_range(from..to)
    };

    state.store(&RETURN_REF, DynValue::F32(val))
}

pub(super) fn pow(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;
    let power = load_single(state, &Ref::Local(LocalID(2)))?;

    state.store(&RETURN_REF, DynValue::F32(val.powf(power)))
}

pub(super) fn sqrt(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;
    
    state.store(&RETURN_REF, DynValue::F32(val.sqrt()))
}

pub(super) fn sin(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.sin()))
}

pub(super) fn arc_sin(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.asin()))
}

pub(super) fn cos(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.cos()))
}

pub(super) fn arc_cos(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.acos()))
}

pub(super) fn tan(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.tan()))
}

pub(super) fn arc_tan(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;

    state.store(&RETURN_REF, DynValue::F32(val.atan()))
}

pub(super) fn infinity(state: &mut Interpreter) -> ExecResult<()> {
    state.store(&RETURN_REF, DynValue::F32(f32::INFINITY))
}

pub(super) fn is_infinite(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?; 
    state.store(&RETURN_REF, DynValue::Bool(val.is_infinite()))
}

pub(super) fn nan(state: &mut Interpreter) -> ExecResult<()> {
    state.store(&RETURN_REF, DynValue::F32(f32::NAN))
}

pub(super) fn is_nan(state: &mut Interpreter) -> ExecResult<()> {
    let val = load_single(state, &Ref::Local(LocalID(1)))?;
    state.store(&RETURN_REF, DynValue::Bool(val.is_nan()))
}

pub fn system_funcs(
    metadata: &Metadata
) -> impl IntoIterator<Item=(&'static str, BuiltinFn, Type, Vec<Type>)> {
    let array_of_ptr = metadata
        .find_dyn_array_struct(&Type::Nothing.ptr())
        .expect("array of raw pointer type must exist in metadata");
    
    let items = [
        ("Int8ToStr", i8_to_str as BuiltinFn, Type::string_ptr(), vec![
            Type::I8
        ]),
        ("UInt8ToStr", u8_to_str, Type::string_ptr(), vec![
            Type::U8
        ]),
        ("Int16ToStr", i16_to_str, Type::string_ptr(), vec![
            Type::I16
        ]),
        ("UInt16ToStr", u16_to_str, Type::string_ptr(), vec![
            Type::U16
        ]),
        ("Int32ToStr", i32_to_str, Type::string_ptr(), vec![
            Type::I32
        ]),
        ("UInt32ToStr", u32_to_str, Type::string_ptr(), vec![
            Type::U32
        ]),
        ("Int64ToStr", i64_to_str, Type::string_ptr(), vec![
            Type::I64
        ]),
        ("UInt64ToStr", u64_to_str, Type::string_ptr(), vec![
            Type::U64
        ]),
        ("NativeIntToStr", isize_to_str, Type::string_ptr(), vec![
            Type::ISize
        ]),
        ("NativeUIntToStr", usize_to_str, Type::string_ptr(), vec![
            Type::USize
        ]),
        ("PointerToStr", pointer_to_str, Type::string_ptr(), vec![
            Type::Nothing.ptr()
        ]),
        ("RealToStr", real_to_str, Type::string_ptr(), vec![
            Type::F32
        ]),
        ("StrToInt", str_to_int, Type::I32, vec![
            STRING_TYPE
        ]),
        ("Write", write, Type::Nothing, vec![
            STRING_TYPE
        ]),
        ("WriteLn", write_ln, Type::Nothing, vec![
            STRING_TYPE
        ]),
        ("ReadLn", read_ln, Type::string_ptr(), vec![]), 
        ("GetMem", get_mem, Type::U8.ptr(), vec![
            Type::I32
        ]),
        ("FreeMem", free_mem, Type::Nothing, vec![
            Type::U8.ptr()
        ]),
        ("ArrayLengthInternal", array_length, Type::I32, vec![
            Type::any()
        ]),
        ("ArraySetLengthInternal", set_length, Type::any(), vec![
            Type::any(), 
            Type::I32, 
            Type::any()
        ]),
        ("InvokeMethod", invoke_method, Type::Nothing, vec![
            METHODINFO_TYPE,
            Type::Nothing.ptr(),
            Type::class_ptr(array_of_ptr),
            Type::Nothing.ptr(),
        ]),
        ("FindTypeInfo", find_type_info, TYPEINFO_TYPE, vec![Type::string_ptr()]),
        ("GetTypeInfoCount", get_type_info_count, Type::I32, vec![]),
        ("GetTypeInfoByIndex", get_type_info_by_index, TYPEINFO_TYPE, vec![Type::I32]),
        ("GetObjectTypeInfo", get_object_type_info, TYPEINFO_TYPE, vec![Type::any()]),
        ("RandomInteger", random_integer, Type::I32, vec![
            Type::I32, Type::I32
        ]),
        ("RandomSingle", random_single, Type::F32, vec![
            Type::F32, Type::F32
        ]),
        ("Pow", pow, Type::F32, vec![
            Type::F32, Type::F32
        ]),
        ("Sqrt", sqrt, Type::F32, vec![
            Type::F32
        ]),
        ("Sin", sin, Type::F32, vec![
            Type::F32
        ]),
        ("ArcSin", arc_sin, Type::F32, vec![
            Type::F32
        ]),
        ("Cos", cos, Type::F32, vec![
            Type::F32
        ]),
        ("ArcCos", arc_cos, Type::F32, vec![
            Type::F32
        ]),
        ("Tan", tan, Type::F32, vec![
            Type::F32
        ]),
        ("ArcTan", arc_tan, Type::F32, vec![
            Type::F32
        ]),
        ("Infinity", infinity, Type::F32, vec![]),
        ("IsInfinite", is_infinite, Type::Bool, vec![
            Type::F32
        ]),
        ("NaN", nan, Type::F32, vec![]),
        ("IsNaN", is_nan, Type::Bool, vec![
            Type::F32
        ])
    ];

    items
}
