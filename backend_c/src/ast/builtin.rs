use crate::ast::{BuiltinName, Type, TypeDefName, Unit};
use crate::ir;

pub fn system_funcs(unit: &Unit) -> Vec<(&'static str, BuiltinName, Type, Vec<Type>)> {
    let string_ty = Type::DefinedType(TypeDefName::Struct(ir::STRING_ID)).ptr();

    let typeinfo_ty = Type::DefinedType(TypeDefName::Struct(ir::TYPEINFO_ID)).ptr();
    let funcinfo_ty = Type::DefinedType(TypeDefName::Struct(ir::FUNCINFO_ID)).ptr();

    vec![
        ("Int8ToStr", BuiltinName::Int8ToStr, string_ty.clone(), vec![
            Type::SChar
        ]),
        ("UInt8ToStr", BuiltinName::ByteToStr, string_ty.clone(), vec![
            Type::UChar
        ]),
        ("Int16ToStr", BuiltinName::Int16ToStr, string_ty.clone(), vec![
            Type::Int16
        ]),
        ("UInt16ToStr", BuiltinName::UInt16ToStr, string_ty.clone(), vec![
            Type::UInt16
        ]),
        ("Int32ToStr", BuiltinName::IntToStr, string_ty.clone(), vec![
            Type::Int32
        ]),
        ("UInt32ToStr", BuiltinName::UInt32ToStr, string_ty.clone(), vec![
            Type::UInt32
        ]),
        ("Int64ToStr", BuiltinName::Int64ToStr, string_ty.clone(), vec![
            Type::Int64
        ]),
        ("UInt64ToStr", BuiltinName::UInt64ToStr, string_ty.clone(), vec![
            Type::UInt64
        ]),
        ("NativeIntToStr", BuiltinName::NativeIntToStr, string_ty.clone(), vec![
            Type::PtrDiffType
        ]),
        ("NativeUIntToStr", BuiltinName::NativeUIntToStr, string_ty.clone(), vec![
            Type::SizeType
        ]),
        ("PointerToStr", BuiltinName::PointerToStr, string_ty.clone(), vec![
            Type::Void.ptr()
        ]),
        ("RealToStr", BuiltinName::RealToStr, string_ty.clone(), vec![
            Type::Float
        ]),
        ("StrToInt", BuiltinName::StrToInt, Type::Int32, vec![
            string_ty.clone()
        ]),
        ("GetMem", BuiltinName::GetMem, Type::UChar.ptr(), vec![
            Type::Int32
        ]),
        ("FreeMem", BuiltinName::FreeMem, Type::Void, vec![
            Type::UChar.ptr()
        ]),
        ("WriteLn", BuiltinName::WriteLn, Type::Void, vec![
            string_ty.clone()
        ]),
        ("Write", BuiltinName::Write, Type::Void, vec![
            string_ty.clone()
        ]),
        ("ReadLn", BuiltinName::ReadLn, string_ty.clone(), vec![]),
        ("ArrayLengthInternal", BuiltinName::ArrayLengthInternal, Type::Int32, vec![
            Type::Rc.ptr(),
        ]),
        ("ArrayCreateInternal", BuiltinName::ArrayCreateInternal, Type::Void, vec![
            Type::Rc.ptr().ptr(),
            Type::Int32,
        ]),

        ("FindTypeInfo", BuiltinName::FindTypeInfo, typeinfo_ty.clone(), vec![string_ty.clone()]),
        ("GetTypeInfoCount", BuiltinName::GetTypeInfoCount, Type::Int32, vec![]),
        ("GetTypeInfoByIndex", BuiltinName::GetTypeInfoByIndex, typeinfo_ty.clone(), vec![Type::Int32]),
        ("GetObjectTypeInfo", BuiltinName::GetObjectTypeInfo, typeinfo_ty.clone(), vec![Type::Rc.ptr()]),

        ("FindFunctionInfo", BuiltinName::FindFuncInfo, funcinfo_ty.clone(), vec![string_ty.clone()]),
        ("GetFunctionInfoCount", BuiltinName::GetFuncInfoCount, Type::Int32, vec![]),
        ("GetFunctionInfoByIndex", BuiltinName::GetFuncInfoByIndex, funcinfo_ty.clone(), vec![Type::Int32]),
        ("InvokeMethod", BuiltinName::InvokeMethod, Type::object_ptr(), vec![
            Type::from_ir_struct(ir::METHODINFO_ID).ptr(),
            Type::object_ptr(),
            Type::dyn_array_ptr(unit.object_array_id),
        ]),
        ("InvokeFunction", BuiltinName::InvokeFunction, Type::object_ptr(), vec![
            Type::from_ir_struct(ir::FUNCINFO_ID).ptr(),
            Type::dyn_array_ptr(unit.object_array_id),
        ]),
        ("RandomInteger", BuiltinName::RandomInteger, Type::Int32, vec![
            Type::Int32,
            Type::Int32]),
        ("RandomSingle", BuiltinName::RandomSingle, Type::Float, vec![
            Type::Float,
            Type::Float
        ]),
        ("Time", BuiltinName::Time, Type::Double, vec![]),
        ("Pow", BuiltinName::Pow, Type::Float, vec![
            Type::Float, Type::Float
        ]),
        ("Sqrt", BuiltinName::Sqrt, Type::Float, vec![
            Type::Float
        ]),
        ("Sin", BuiltinName::Sin, Type::Float, vec![
            Type::Float
        ]),
        ("ArcSin", BuiltinName::ArcSin, Type::Float, vec![
            Type::Float
        ]),
        ("Cos", BuiltinName::Cos, Type::Float, vec![
            Type::Float
        ]),
        ("ArcCos", BuiltinName::ArcCos, Type::Float, vec![
            Type::Float
        ]),
        ("Tan", BuiltinName::Tan, Type::Float, vec![
            Type::Float
        ]),
        ("ArcTan", BuiltinName::ArcTan, Type::Float, vec![
            Type::Float
        ]),
        ("Infinity", BuiltinName::Infinity, Type::Float, vec![]),
        ("NaN", BuiltinName::NaN, Type::Float, vec![]),
        ("IsInfinite", BuiltinName::IsInfinite, Type::Bool, vec![
            Type::Float
        ]),
        ("IsNaN", BuiltinName::IsNaN, Type::Bool, vec![
            Type::Float
        ]),
    ]
}
    
