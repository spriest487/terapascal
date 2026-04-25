use crate::ast::BuiltinName;
use crate::ir;

pub fn system_funcs() -> Vec<(&'static str, BuiltinName, ir::Type, Vec<ir::Type>)> {
    let string_ty = ir::Type::string();

    let typeinfo_ty = ir::Type::type_info();
    let funcinfo_ty = ir::Type::func_info();

    vec![
        ("Int8ToStr", BuiltinName::Int8ToStr, string_ty.clone(), vec![
            ir::Type::I8
        ]),
        ("UInt8ToStr", BuiltinName::ByteToStr, string_ty.clone(), vec![
            ir::Type::U8
        ]),
        ("Int16ToStr", BuiltinName::Int16ToStr, string_ty.clone(), vec![
            ir::Type::I16
        ]),
        ("UInt16ToStr", BuiltinName::UInt16ToStr, string_ty.clone(), vec![
            ir::Type::U16
        ]),
        ("Int32ToStr", BuiltinName::IntToStr, string_ty.clone(), vec![
            ir::Type::I32
        ]),
        ("UInt32ToStr", BuiltinName::UInt32ToStr, string_ty.clone(), vec![
            ir::Type::U32
        ]),
        ("Int64ToStr", BuiltinName::Int64ToStr, string_ty.clone(), vec![
            ir::Type::I64
        ]),
        ("UInt64ToStr", BuiltinName::UInt64ToStr, string_ty.clone(), vec![
            ir::Type::U64
        ]),
        ("NativeIntToStr", BuiltinName::NativeIntToStr, string_ty.clone(), vec![
            ir::Type::ISize
        ]),
        ("NativeUIntToStr", BuiltinName::NativeUIntToStr, string_ty.clone(), vec![
            ir::Type::USize
        ]),
        ("PointerToStr", BuiltinName::PointerToStr, string_ty.clone(), vec![
            ir::Type::Nothing.ptr()
        ]),
        ("RealToStr", BuiltinName::Real32ToStr, string_ty.clone(), vec![
            ir::Type::F32
        ]),
        ("Real64ToStr", BuiltinName::Real64ToStr, string_ty.clone(), vec![
            ir::Type::F32
        ]),
        ("StrToInt", BuiltinName::StrToInt, ir::Type::I32, vec![
            string_ty.clone()
        ]),
        ("GetMem", BuiltinName::GetMem, ir::Type::U8.ptr(), vec![
            ir::Type::I32
        ]),
        ("FreeMem", BuiltinName::FreeMem, ir::Type::Nothing, vec![
            ir::Type::U8.ptr()
        ]),
        ("WriteLn", BuiltinName::WriteLn, ir::Type::Nothing, vec![
            string_ty.clone()
        ]),
        ("Write", BuiltinName::Write, ir::Type::Nothing, vec![
            string_ty.clone()
        ]),
        ("ReadLn", BuiltinName::ReadLn, string_ty.clone(), vec![]),
        ("ArrayLengthInternal", BuiltinName::ArrayLengthInternal, ir::Type::I32, vec![
            ir::ANY_TYPE,
        ]),
        ("ArrayCreateInternal", BuiltinName::ArrayCreateInternal, ir::Type::Nothing, vec![
            ir::ANY_TYPE.ptr(),
            ir::Type::I32,
        ]),

        ("FindTypeInfo", BuiltinName::FindTypeInfo, typeinfo_ty.clone(), vec![string_ty.clone()]),
        ("GetTypeInfoCount", BuiltinName::GetTypeInfoCount, ir::Type::I32, vec![]),
        ("GetTypeInfoByIndex", BuiltinName::GetTypeInfoByIndex, typeinfo_ty.clone(), vec![ir::Type::I32]),
        ("GetObjectTypeInfo", BuiltinName::GetObjectTypeInfo, typeinfo_ty.clone(), vec![ir::ANY_TYPE]),

        ("FindFunctionInfo", BuiltinName::FindFuncInfo, funcinfo_ty.clone(), vec![string_ty.clone()]),
        ("GetFunctionInfoCount", BuiltinName::GetFuncInfoCount, ir::Type::I32, vec![]),
        ("GetFunctionInfoByIndex", BuiltinName::GetFuncInfoByIndex, funcinfo_ty.clone(), vec![ir::Type::I32]),
        ("InvokeMethod", BuiltinName::InvokeMethod, ir::ANY_TYPE, vec![
            ir::Type::method_info(),
            ir::ANY_TYPE.temp_ref(),
            ir::ANY_TYPE.dyn_array(),
            ir::Type::I32.temp_ref(),
        ]),
        ("InvokeFunction", BuiltinName::InvokeFunction, ir::ANY_TYPE, vec![
            ir::Type::func_info(),
            ir::ANY_TYPE.dyn_array(),
            ir::Type::I32.temp_ref(),
        ]),
        ("RandomInteger", BuiltinName::RandomInteger, ir::Type::I32, vec![
            ir::Type::I32,
            ir::Type::I32]),
        ("RandomSingle", BuiltinName::RandomSingle, ir::Type::F32, vec![
            ir::Type::F32,
            ir::Type::F32
        ]),
        ("Time", BuiltinName::Time, ir::Type::F64, vec![]),
        ("Pow", BuiltinName::Pow, ir::Type::F32, vec![
            ir::Type::F32, ir::Type::F32
        ]),
        ("Sqrt", BuiltinName::Sqrt, ir::Type::F32, vec![
            ir::Type::F32
        ]),
        ("Sin", BuiltinName::Sin, ir::Type::F32, vec![
            ir::Type::F32
        ]),
        ("ArcSin", BuiltinName::ArcSin, ir::Type::F32, vec![
            ir::Type::F32
        ]),
        ("Cos", BuiltinName::Cos, ir::Type::F32, vec![
            ir::Type::F32
        ]),
        ("ArcCos", BuiltinName::ArcCos, ir::Type::F32, vec![
            ir::Type::F32
        ]),
        ("Tan", BuiltinName::Tan, ir::Type::F32, vec![
            ir::Type::F32
        ]),
        ("ArcTan", BuiltinName::ArcTan, ir::Type::F32, vec![
            ir::Type::F32
        ]),
        ("Infinity", BuiltinName::Infinity, ir::Type::F32, vec![]),
        ("NaN", BuiltinName::NaN, ir::Type::F32, vec![]),
        ("IsInfinite", BuiltinName::IsInfinite, ir::Type::Bool, vec![
            ir::Type::F32
        ]),
        ("IsNaN", BuiltinName::IsNaN, ir::Type::Bool, vec![
            ir::Type::F32
        ]),
    ]
}
    
