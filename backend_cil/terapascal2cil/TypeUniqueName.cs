using Terapascal.IR;

namespace Terapascal.CIL;

public static class TypeDeclName {
    public static string GetTypeDeclName(this IR.IType type) {
        switch (type) {
            case ArrayType arrayType: break;
            case BoolType boolType: break;
            case F32Type f32Type: break;
            case F64Type f64Type: break;
            case FlagsType flagsType: break;
            case FunctionType functionType: break;
            case I16Type i16Type: break;
            case I32Type i32Type: break;
            case I64Type i64Type: break;
            case I8Type i8Type: break;
            case ISizeType sizeType: break;
            case NothingType nothingType: break;
            case PointerType pointerType: break;
            case RcPointerType rcPointerType: break;
            case RcWeakPointerType rcWeakPointerType: break;
            case StructType structType: break;
            case TempRefType tempRefType: break;
            case U16Type u16Type: break;
            case U32Type u32Type: break;
            case U64Type u64Type: break;
            case U8Type u8Type: break;
            case USizeType uSizeType: break;
            case VariantType variantType: break;
            default: throw new ArgumentOutOfRangeException(nameof(type));
        }
    }
}
