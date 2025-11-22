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

    [Key("set_aliases")]
    public required OrderedDictionary<SetAliasID, SetAliasDef> SetAliases { get; init; }

    [Key("function_info")]
    public required OrderedDictionary<FunctionID, FunctionInfo> Functions { get; init; }

    [Key("closures")]
    public required SortedDictionary<TypeDefID, List<TypeDefID>> Closures {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("function_static_closures")]
    public required IReadOnlyDictionary<FunctionID, StaticClosureID> FunctionStaticClosures {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("type_info")]
    public required Dictionary<IType, TypeInfo> TypeInfo {
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
}

[MessagePackObject]
public class TypeInfo {
    [Key("name")]
    public StringID? Name { get; init; }
    
    [Key("debug_name")]
    public string? DebugName { get; init; }
    
    [Key("dtor")]
    public FunctionID? Destructor { get; init; }

    [Key("methods")]
    public required IReadOnlyList<RuntimeMethod> Methods {
        get;
        init => field = value.ToArrayNonNull();
    }
    
    [Key("flags")]
    public ulong Flags { get; init; }
}

[MessagePackObject]
public class RuntimeMethod {
    [Key("name")]
    public required StringID Name { get; init; }

    [Key("instance_ty")]
    public required IType InstanceType {
        get;
        set => field = value ?? IType.Nothing;
    }

    [Key("function")]
    public FunctionID? Function { get; init; }

    [Key("result_ty")]
    public required IType ResultType {
        get;
        set => field = value ?? IType.Nothing;
    }
    
    [Key("params")]
    public required IReadOnlyList<IType> ParameterTypes {
        get;
        init => field = value.ToArrayNonNull();
    }
}
