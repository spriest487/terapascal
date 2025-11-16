use crate::func::BuiltinFn;
use crate::ir::*;
use crate::DynValue;
use crate::ExecError;
use crate::ExecResult;
use crate::Interpreter;
use crate::Pointer;
use rand::Rng;
use std::env::consts::OS;
use std::{fmt, iter};
use std::io;
use std::io::BufRead;
use std::io::Write;
use std::time::Duration;
use std::time::SystemTime;

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

/// %1: <any dyn array ref> -> %0: Integer
pub(super) fn array_length(state: &mut Interpreter) -> ExecResult<()> {
    let array_arg = LocalID(1);
    
    let array_ptr = load_pointer(state, &array_arg.to_ref())?;
    let array_header = state.marshaller.unmarshal_dyn_array_header_at(&array_ptr)?;
    
    let len = i32::try_from(array_header.len)
        .map_err(|_| ExecError::illegal_state("length of array was not an i32"))?;
    
    state.store(&RETURN_REF, DynValue::I32(len))?;

    Ok(())
}

/// %0: &Any <any dyn array ref>; %1: I32;
pub(super) fn array_create(state: &mut Interpreter) -> ExecResult<()> {
    let array_ref_arg = LocalID(0);
    let new_len_arg = LocalID(1);

    let new_len = load_integer(state, &new_len_arg.to_ref())?;

    let mut array_ptr = load_pointer(state, &array_ref_arg.to_deref())?;
    let array_header = state.marshaller().unmarshal_dyn_array_header_at(&array_ptr)?;
    
    if array_header.rc.is_immortal() {
        return Err(ExecError::illegal_state("array_create: array is immortal and cannot be resized"));
    }
    
    let Type::Array { element: element_type, dim: 0 } = &array_header.rc.marshal_type else {
        return Err(ExecError::illegal_state("array_create: array pointer points to an invalid array object"));
    };

    let default_val = state.default_val(element_type)?;
    let elements = iter::repeat(default_val).take(new_len as usize).collect();

    state.release_dyn_val(&DynValue::Pointer(array_ptr.clone()), false)?;

    // keep the exact same RC state
    array_ptr = state.new_dyn_array_with_header(&element_type, elements, array_header.rc.clone())?;
    
    state.store(&array_ref_arg.to_deref(), DynValue::Pointer(array_ptr))?;

    Ok(())
}

fn invoke_method(state: &mut Interpreter) -> ExecResult<()> {
    let method_ptr = load_pointer(state, &Ref::Local(LocalID(0)))?;
    let instance_ptr = load_pointer(state, &Ref::Local(LocalID(1)))?;
    let args_ptr = load_pointer(state, &Ref::Local(LocalID(2)))?;
    let args_count = load_integer(state, &Ref::Local(LocalID(3)))?;
    let result_ptr_arg = load_pointer(state, &Ref::Local(LocalID(4)))?;

    let (method_info_val,_) = state.load_object(&method_ptr)?;

    let method_global_index = method_info_val[METHODINFO_IMPL_FIELD]
        .as_pointer()
        .ok_or_else(|| ExecError::illegal_state("bad type: impl field of MethodInfo must be a pointer"))?
        .addr;

    let runtime_method = state.runtime_methods
        .get(method_global_index)
        .ok_or_else(|| ExecError::illegal_state("InvokeMethod called for method with invalid impl pointer"))?
        .clone();
    
    match runtime_method.function {
        Some(func_id) => {
            let type_info_ptr = method_info_val[METHODINFO_OWNER_FIELD]
                .as_pointer()
                .ok_or_else(|| {
                    let msg = format!("bad type: expected owner pointer at field {METHODINFO_OWNER_FIELD} of method info");
                    ExecError::illegal_state(msg)
                })?;

            let (type_info_val, _) = state.load_object(&type_info_ptr)?;
            let type_name_ptr = type_info_val[TYPEINFO_NAME_FIELD]
                .as_pointer()
                .ok_or_else(|| {
                    let msg = format!("bad type: expected string pointer at field {TYPEINFO_NAME_FIELD} of type info");
                    ExecError::illegal_state(msg)
                })?;

            let type_name = state.read_string_indirect(type_name_ptr)?;

            let method_name_ptr = method_info_val[METHODINFO_NAME_FIELD]
                .as_pointer()
                .ok_or_else(|| {
                    let msg = format!("bad type: expected string pointer at field {METHODINFO_NAME_FIELD} of method info");
                    ExecError::illegal_state(msg)
                })?;
            let method_name = state.read_string_indirect(method_name_ptr)?;
            
            state.runtime_invoke(
                func_id,
                &instance_ptr,
                &runtime_method.instance_ty,
                &type_name,
                args_ptr,
                args_count,
                &method_name,
                &runtime_method.params,
                &runtime_method.result_ty,
                &result_ptr_arg,
            )
        }

        None => {
            Err(ExecError::illegal_state("InvokeMethod called for abstract method"))
        }
    }
}

fn invoke_func(state: &mut Interpreter) -> ExecResult<()> {
    let func_ptr = load_pointer(state, &Ref::Local(LocalID(0)))?;
    let args_ptr = load_pointer(state, &Ref::Local(LocalID(1)))?;
    let args_count = load_integer(state, &Ref::Local(LocalID(2)))?;
    let result_ptr_arg = load_pointer(state, &Ref::Local(LocalID(3)))?;

    let (func_info_val,_) = state.load_object(&func_ptr)?;
    let impl_val = func_info_val[FUNCINFO_IMPL_FIELD]
        .as_pointer()
        .ok_or_else(|| ExecError::illegal_state("impl field must be a pointer"))?
        .addr;

    let func_id = FunctionID(impl_val);
    let func = state.functions[&func_id].clone();
    let func_name = func.name.as_ref();

    state.runtime_invoke(
        func_id,
        &Pointer::nil(Type::Nothing),
        &Type::Nothing,
        "",
        args_ptr,
        args_count,
        func_name,
        func.func.param_tys(),
        func.func.return_ty(),
        &result_ptr_arg
    )
}

fn find_type_info(state: &mut Interpreter) -> ExecResult<()> {
    let name_arg = state.read_string(&Ref::Local(LocalID(1)))?;
    
    let result = match state.typeinfo_map.find_by_name(&name_arg).cloned() {
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
    let count = i32::try_from(state.typeinfo_map.items().len())
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
        .and_then(|i| state.typeinfo_map.items().get(i).cloned())
        .ok_or_else(|| ExecError::illegal_state(format!("illegal TypeInfo index: {index}")))?;
    
    let type_info_ptr = state.load(&Ref::Global(type_info_ref))?;
    state.store(&RETURN_REF, type_info_ptr)?;
    
    Ok(())
}

fn get_object_type_info(state: &mut Interpreter) -> ExecResult<()> {
    let obj_ptr_arg = LocalID(1);
    let obj_ptr = load_pointer(state, &Ref::Local(obj_ptr_arg))?;
    
    let obj_header = state.load_object_header(&obj_ptr)?;
    
    match &obj_header.marshal_type {
        Type::Struct(class_id) => {
            let type_info_ref = state
                .get_class_runtime_type_ref(*class_id)
                .ok_or_else(|| {
                    let message = format!("missing runtime type info for class struct {class_id}");
                    ExecError::illegal_state(message)
                })?;

            let type_info_ptr = state.load(&Ref::Global(type_info_ref))?;
            state.store(&RETURN_REF, type_info_ptr)?;
            
            Ok(())
        }
        
        _ => {
            let type_name = state.metadata.pretty_ty_name(&obj_header.marshal_type);
            let message = format!("missing runtime type info for object type {}", type_name);
            Err(ExecError::illegal_state(message))
        }
    }
}

fn find_func_info(state: &mut Interpreter) -> ExecResult<()> {
    let name_arg = state.read_string(&Ref::Local(LocalID(1)))?;

    let result = match state.funcinfo_map.find_by_name(&name_arg).cloned() {
        Some(funcinfo_global) => {
            state.load(&Ref::Global(funcinfo_global))?
        }

        None => {
            DynValue::nil(FUNCINFO_TYPE)
        }
    };

    state.store(&RETURN_REF, result)?;

    Ok(())
}

fn get_func_info_count(state: &mut Interpreter) -> ExecResult<()> {
    let count = i32::try_from(state.funcinfo_map.items().len())
        .unwrap_or(i32::MAX);

    state.store(&RETURN_REF, DynValue::I32(count))
}

fn get_func_info_by_index(state: &mut Interpreter) -> ExecResult<()> {
    let index_param_local = Ref::Local(LocalID(1));

    let index = state.load(&index_param_local)?
        .as_i32()
        .ok_or_else(|| {
            ExecError::illegal_state("parameter to get_func_info_by_index must be i32")
        })?;

    let funcinfo_ref = usize::try_from(index)
        .ok()
        .and_then(|i| state.funcinfo_map.items().get(i).cloned())
        .ok_or_else(|| ExecError::illegal_state(format!("illegal FunctionInfo index: {index}")))?;

    let funcinfo_ptr = state.load(&Ref::Global(funcinfo_ref))?;
    state.store(&RETURN_REF, funcinfo_ptr)?;

    Ok(())
}

fn load_pointer(state: &mut Interpreter, at: &Ref) -> ExecResult<Pointer> {
    let val = state.load(at)?;

    val.as_pointer()
        .cloned()
        .ok_or_else(|| {
            let msg = format!("bad type: expected pointer at {at} (was: {})", val.value_type_category());
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

pub(super) fn time(state: &mut Interpreter) -> ExecResult<()> {
    let since_epoch = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or(Duration::ZERO);

    state.store(&RETURN_REF, DynValue::F64(since_epoch.as_secs_f64()))
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

pub fn system_funcs() -> impl IntoIterator<Item=(&'static str, BuiltinFn, Type, Vec<Type>)> {    
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
        ("ArrayCreateInternal", array_create, Type::Nothing, vec![
            Type::any().temp_ref(), 
            Type::I32, 
        ]),
        ("InvokeMethod", invoke_method, Type::any(), vec![
            METHODINFO_TYPE,
            Type::any(),
            Type::any().dyn_array(),
        ]),
        ("InvokeFunction", invoke_func, Type::any(), vec![
            FUNCINFO_TYPE,
            Type::any().dyn_array(),
        ]),
        
        ("FindTypeInfo", find_type_info, TYPEINFO_TYPE, vec![Type::string_ptr()]),
        ("GetTypeInfoCount", get_type_info_count, Type::I32, vec![]),
        ("GetTypeInfoByIndex", get_type_info_by_index, TYPEINFO_TYPE, vec![Type::I32]),
        ("GetObjectTypeInfo", get_object_type_info, TYPEINFO_TYPE, vec![Type::any()]),
        
        ("FindFunctionInfo", find_func_info, FUNCINFO_TYPE, vec![Type::string_ptr()]),
        ("GetFunctionInfoCount", get_func_info_count, Type::I32, vec![]),
        ("GetFunctionInfoByIndex", get_func_info_by_index, FUNCINFO_TYPE, vec![Type::I32]),

        ("RandomInteger", random_integer, Type::I32, vec![
            Type::I32, Type::I32
        ]),
        ("RandomSingle", random_single, Type::F32, vec![
            Type::F32, Type::F32
        ]),
        ("Time", time, Type::F64, vec![]),
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
