namespace Terapascal.CIL;

public static class TypeUniqueName {
    public static string GetUniqueName(this IR.IType type) {
        return type switch {
            IR.ArrayType arrayType => $"Array{arrayType.Length}_of_{arrayType.Element.GetUniqueName()}",
            IR.BoolType => "Bool",
            IR.F32Type => "F32",
            IR.F64Type => "F64",
            IR.FlagsType(var setID) => $"Flags_{setID.ID}",
            IR.FunctionType(var defID) => $"FunctionType_{defID.ID}",
            IR.I16Type => "I16",
            IR.I32Type => "I32",
            IR.I64Type => "I64",
            IR.I8Type => "I8",
            IR.ISizeType => "ISize",
            IR.NothingType => "Nothing",
            IR.PointerType(var derefType) => $"Ptr_{derefType.GetUniqueName()}",
            IR.ObjectType(var classID) => $"Object_{classID.GetUniqueName()}",
            IR.WeakObjectType(var classID) => $"WeakObject_{classID.GetUniqueName()}",
            IR.StructType(var id) => $"Struct_{id.ID}",
            IR.TempRefType tempRefType => $"Ref_{tempRefType.GetUniqueName()}",
            IR.U16Type => "U16",
            IR.U32Type => "U32",
            IR.U64Type => "U64",
            IR.U8Type => "U8",
            IR.USizeType => "USize",
            IR.VariantType(var id) => $"Variant_{id.ID}",
            _ => throw new NotSupportedException(nameof(type)),
        };
    }

    public static string GetUniqueName(this IR.IObjectID classID) {
        return classID switch {
            IR.AnyObjectID => "Any",
            IR.ClassObjectID(var id) => $"Class_{id.ID}",
            IR.ClosureObjectID(var id) => $"AnyClosure_{id.ID}",
            IR.InterfaceObjectID(var id) => $"Interface_{id.ID}",
            IR.ArrayObjectID(var elementType) => $"DynArray_{elementType.GetUniqueName()}",
            IR.BoxObjectID(var valueType) => $"Box_{valueType.GetUniqueName()}",
            _ => throw new NotSupportedException(nameof(classID)),
        };
    }
}
