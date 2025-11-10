using System.Diagnostics.CodeAnalysis;
using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class Metadata {
    [Key("type_decls")]
    public required OrderedDictionary<TypeDefID, ITypeDecl> TypeDecls { get; init; }

    [Key("string_literals")]
    public required OrderedDictionary<StringID, string> StringLiterals { get; init; }

    [Key("ifaces")]
    public required OrderedDictionary<InterfaceID, IInterfaceDecl> Interfaces { get; init; }

    [Key("variables")]
    public required OrderedDictionary<VariableID, IType> Variables { get; init; }

    [Key("dtors")]
    public required SortedDictionary<TypeDefID, FunctionID> Destructors {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("set_aliases")]
    public required OrderedDictionary<SetAliasID, SetAliasDef> SetAliases { get; init; }

    [Key("functions")]
    public required OrderedDictionary<FunctionID, FunctionDecl> Functions { get; init; }

    [Key("closures")]
    public required IReadOnlyList<TypeDefID> Closures {
        get;
        init => field = value!.ToArrayNonNull();
    }

    [Key("function_static_closures")]
    public required IReadOnlyDictionary<FunctionID, StaticClosureID> FunctionStaticClosures {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("dyn_array_structs")]
    public required OrderedDictionary<IType, TypeDefID> DynArrayStructs { get; init; }

    [Key("dyn_array_classes")]
    public required Dictionary<IType, DynArrayClass> DynArrayClasses {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("runtime_types")]
    public required Dictionary<IType, RuntimeType> RuntimeTypes {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("bounds_check_functions")]
    public required Dictionary<IType, FunctionID> BoundsCheckFunctions {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("tag_counts")]
    public required Dictionary<ITagLocation, ulong> TagCounts {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    public bool FindFunctionType(FunctionSig withSig, out TypeDefID id) {
        foreach (var (declID, typeDecl) in this.TypeDecls) {
            if (typeDecl is not DefTypeDecl(FunctionTypeDef(var sig))) {
                continue;
            }

            if (sig.Equals(withSig)) {
                id = declID;
                return true;
            }
        }

        id = default;
        return false;
    }

    public bool FindVariantDef(TypeDefID id, [NotNullWhen(true)] out VariantDef? def) {
        def = null;
        if (!this.TypeDecls.TryGetValue(id, out var typeDecl)) {
            return false;
        }

        if (typeDecl is not DefTypeDecl(VariantTypeDef(var declDef))) {
            return false;
        }

        def = declDef;
        return true;
    }
    
    public bool FindStructDef(TypeDefID id, [NotNullWhen(true)] out StructDef? def) {
        def = null;
        if (!this.TypeDecls.TryGetValue(id, out var typeDecl)) {
            return false;
        }

        if (typeDecl is not DefTypeDecl(StructTypeDef(var declDef))) {
            return false;
        }

        def = declDef;
        return true;
    }

    public IType? GetDynArrayTypeElement(TypeDefID arrayClassID) {
        foreach (var (type, id) in this.DynArrayStructs) {
            if (id.Equals(arrayClassID)) {
                return type;
            }
        }

        return null;
    }
}

[MessagePackObject]
public class RuntimeType {
}

[MessagePackObject]
public class DynArrayClass {
}
