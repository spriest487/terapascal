namespace Terapascal.CIL;

public static class TypeUniqueName {
    public static string GetUniqueName(this IR.IType type) {
        return type switch {
            IR.ArrayType arrayType => $"Array{arrayType.Length}_of_{arrayType.Element.GetUniqueName()}",
            IR.BoolType => "Bool",
            IR.F32Type => "F32",
            IR.F64Type => "F64",
            IR.FlagsType(var setID, _) => $"Flags_{setID.ID}",
            IR.FunctionType(var defID) => $"FunctionType_{defID.ID}",
            IR.I16Type => "I16",
            IR.I32Type => "I32",
            IR.I64Type => "I64",
            IR.I8Type => "I8",
            IR.ISizeType => "ISize",
            IR.NothingType => "Nothing",
            IR.PointerType(var derefType) => $"Ptr_{derefType.GetUniqueName()}",
            IR.RcPointerType(var classID) => $"Object_{classID.GetUniqueName()}",
            IR.RcWeakPointerType(var classID) => $"WeakObject_{classID.GetUniqueName()}",
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

    public static string GetUniqueName(this IR.IVirtualTypeID classID) {
        return classID switch {
            IR.AnyVirtualTypeID => "Any",
            IR.ArrayVirtualTypeID(var elementType) => $"DynArray_{elementType.GetUniqueName()}",
            IR.ClassVirtualTypeID(var id) => $"Class_{id.ID}",
            IR.ClosureVirtualTypeID(var id) => $"AnyClosure_{id.ID}",
            IR.InterfaceVirtualTypeID(var id) => $"Interface_{id.ID}",
            _ => throw new NotSupportedException(nameof(classID)),
        };
    }
}
