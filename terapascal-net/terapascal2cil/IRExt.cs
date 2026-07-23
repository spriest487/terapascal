using Terapascal.Runtime;

namespace Terapascal.CIL;

public static class IRExt {
    extension(IR.IType type) {
        public TypeFlags GetRuntimeTypeFlags(IR.IMetadataSource metadata) => type switch {
            IR.ArrayType => TypeFlags.Array,

            IR.FunctionType => TypeFlags.Function,

            IR.PrimitiveType or IR.StructType or IR.VariantType => TypeFlags.Value,

            IR.ObjectType(IR.ArrayObjectID) => TypeFlags.Array,
            IR.WeakObjectType(IR.ArrayObjectID) => TypeFlags.Weak | TypeFlags.Array,

            IR.ObjectType(IR.AnyClosureObjectID) => TypeFlags.Function,
            IR.WeakObjectType(IR.AnyClosureObjectID) => TypeFlags.Weak | TypeFlags.Function,

            IR.ObjectType(IR.ClassObjectID(var typeDef)) => GetClassTypeFlags(typeDef.DefID, metadata),
            IR.WeakObjectType(IR.ClassObjectID(var typeDef)) => TypeFlags.Weak
                | GetClassTypeFlags(typeDef.DefID, metadata),

            IR.WeakObjectType => TypeFlags.Weak,

            _ => TypeFlags.None,
        };
    }

    private static TypeFlags GetClassTypeFlags(IR.TypeDefID defID, IR.IMetadataSource metadata) {
        if (!metadata.FindStructDef(defID, out var structDef)) {
            return TypeFlags.None;
        }

        return structDef.Identity switch {
            IR.ClosureStructIdentity => TypeFlags.Function,
            _ => TypeFlags.None,
        };
    }
}
