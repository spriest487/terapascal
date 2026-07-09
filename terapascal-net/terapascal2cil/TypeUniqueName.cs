namespace Terapascal.CIL;

internal static class TypeUniqueName {
    internal static string GetUniqueName(this IR.IType type, TypeCache typeCache) {
        return type switch {
            IR.ArrayType arrayType => $"Array{arrayType.Length}_of_{arrayType.Element.GetUniqueName(typeCache)}",
            IR.BoolType => "Bool",
            IR.F32Type => "F32",
            IR.F64Type => "F64",
            IR.FunctionType(_) => $"FunctionType_{typeCache.GetTypeID(type)}",
            IR.I16Type => "I16",
            IR.I32Type => "I32",
            IR.I64Type => "I64",
            IR.I8Type => "I8",
            IR.ISizeType => "ISize",
            IR.NothingType => "Nothing",
            IR.PointerType(var derefType) => $"Ptr_{derefType.GetUniqueName(typeCache)}",
            IR.ObjectType(var classID) => $"Object_{classID.GetUniqueName(typeCache)}",
            IR.WeakObjectType(var classID) => $"WeakObject_{classID.GetUniqueName(typeCache)}",
            IR.StructType(_) => $"Struct_{typeCache.GetTypeID(type)}",
            IR.VariantType(_) => $"Variant_{typeCache.GetTypeID(type)}",
            IR.TempRefType tempRefType => $"Ref_{tempRefType.GetUniqueName(typeCache)}",
            IR.U16Type => "U16",
            IR.U32Type => "U32",
            IR.U64Type => "U64",
            IR.U8Type => "U8",
            IR.USizeType => "USize",
            _ => throw new NotSupportedException(nameof(type)),
        };
    }

    private static string GetUniqueName(this IR.IObjectID classID, TypeCache typeCache) {
        return classID switch {
            IR.AnyObjectID => "Any",
            IR.ClassObjectID(_) => $"Class_{typeCache.GetTypeID(classID.ToObjectType())}",
            IR.AnyClosureObjectID(_) => $"AnyClosure_{typeCache.GetTypeID(classID.ToObjectType())}",
            IR.InterfaceObjectID(_) => $"Interface_{typeCache.GetTypeID(classID.ToObjectType())}",
            IR.ArrayObjectID(var elementType) => $"DynArray_{elementType.GetUniqueName(typeCache)}",
            IR.BoxObjectID(var valueType) => $"Box_{valueType.GetUniqueName(typeCache)}",
            _ => throw new NotSupportedException(nameof(classID)),
        };
    }
}
