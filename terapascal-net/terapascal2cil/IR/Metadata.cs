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
    
    [Key("iface_impls")]
    public required IReadOnlyDictionary<IType, SortedDictionary<InterfaceID, InterfaceImpl>> InterfaceImpls { get; init; }

    [Key("variables")]
    public required OrderedDictionary<VariableID, VariableInfo> Variables { get; init; }

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

    public IEnumerable<(TypeDefID ID, ITypeDef TypeDef)> GetTypeDefs() {
        foreach (var (id, decl) in this.TypeDecls) {
            if (decl is DefTypeDecl(var def)) {
                yield return (id, def);
            }
        }
    }
    
    public IEnumerable<(InterfaceID ID, InterfaceDef InterfaceDef)> GetInterfaceDefs() {
        foreach (var (id, decl) in this.Interfaces) {
            if (decl is DefInterfaceDecl(var def)) {
                yield return (id, def);
            }
        }
    }

    public IEnumerable<(ITagLocation, IReadOnlyList<TagInfo>)> GetAllTags() {
        var typeTags = this.GetTypeDefs()
            .Select(entry => {
                var tags = entry.TypeDef switch {
                    StructTypeDef(var structDef) => structDef.Tags,
                    VariantTypeDef(var variantDef) => variantDef.Tags,
                    _ => [],
                };

                return ((ITagLocation)new TypeDefTagLocation(entry.ID), tags);
            });

        var ifaceTags = this.GetInterfaceDefs()
            .Select(entry => ((ITagLocation)new InterfaceTagLocation(entry.ID), entry.InterfaceDef.Tags));

        var funcTags = this.Functions
            .Select(entry => ((ITagLocation)new FunctionTagLocation(entry.Key), entry.Value.Tags));

        var methodTags = this.TypeInfo.Values
            .SelectMany(typeInfo => typeInfo.Methods)
            .Select(methodInfo => {
                var loc = (ITagLocation)(methodInfo.InstanceType switch {
                    ObjectType(InterfaceObjectID(var ifaceID)) => new InterfaceMethodTagLocation {
                        InterfaceID = ifaceID,
                        MethodIndex = methodInfo.Index,
                    },
                    ObjectType(ClassObjectID(var classID)) => new MethodTagLocation {
                        TypeID = classID,
                        MethodIndex = methodInfo.Index,
                    },
                    StructType(var structID) => new MethodTagLocation {
                        TypeID = structID,
                        MethodIndex = methodInfo.Index,
                    },
                    VariantType(var variantID) => new MethodTagLocation {
                        TypeID = variantID,
                        MethodIndex = methodInfo.Index,
                    },
                    FlagsType(var aliasID) => new MethodTagLocation {
                        TypeID = aliasID,
                        MethodIndex = methodInfo.Index,
                    },
                    _ => throw new InvalidDataException(
                        $"unexpected base type for method: {methodInfo.InstanceType.ToPrettyString(this)}"),
                });

                return (loc, methodInfo.Tags);
            });
        
        return typeTags
            .Concat(ifaceTags)
            .Concat(funcTags)
            .Concat(methodTags);
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
    public required IReadOnlyList<MethodInfo> Methods {
        get;
        init => field = value.ToArrayNonNull();
    }
    
    [Key("flags")]
    public ulong Flags { get; init; }
}

[MessagePackObject]
public class MethodInfo {
    [Key("name")]
    public required StringID Name { get; init; }
    
    [Key("index")]
    public required ulong Index { get; init; }

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

    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }
}

[MessagePackObject]
public class VariableInfo {
    [Key("name")]
    public required NamePath Name { get; init; }
    
    [Key("type")]
    public required IType Type { get; init; }
}
