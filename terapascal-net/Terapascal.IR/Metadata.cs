using System.Diagnostics.CodeAnalysis;
using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class Metadata : IMetadataSource {
    [Key("type_decls")]
    public required OrderedDictionary<TypeDefID, ITypeDecl> TypeDecls { get; init; }

    [Key("string_literals")]
    public required OrderedDictionary<StringID, string> StringLiterals { get; init; }

    [Key("interface_defs")]
    public required OrderedDictionary<InterfaceID, IInterfaceDecl> Interfaces { get; init; }

    [Key("iface_impls")]
    public required Dictionary<IType, Dictionary<InterfaceRef, InterfaceImpl>> InterfaceImpls { get; init; }

    [Key("variables")]
    public required SortedDictionary<VariableID, VariableInfo> Variables { get; init; }

    [Key("constants")]
    public required Dictionary<StringPath, ConstInfo> ConstantInfos { get; init; }

    [Key("type_info")]
    public required Dictionary<IType, TypeInfo> TypeInfo {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("function_info")]
    public required OrderedDictionary<FunctionID, FunctionInfo> Functions { get; init; }

    [Key("closures")]
    public required Dictionary<FunctionSig, List<TypeDefID>> Closures {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    public bool FindVariable(VariableID id, [NotNullWhen(true)] out VariableInfo? variableInfo) {
        return this.Variables.TryGetValue(id, out variableInfo);
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

    public bool FindStringLiteral(StringID id, [NotNullWhen(true)] out string? literal) {
        return this.StringLiterals.TryGetValue(id, out literal);
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

    public bool FindInterfaceDecl(InterfaceID id, [NotNullWhen(true)] out IInterfaceDecl? interfaceDecl) {
        return this.Interfaces.TryGetValue(id, out interfaceDecl);
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
                    ObjectType(InterfaceObjectID(var ifaceRef)) => new InterfaceMethodTagLocation {
                        InterfaceID = ifaceRef.DefID,
                        MethodIndex = methodInfo.Index,
                    },
                    ObjectType(ClassObjectID(var classRef)) => new MethodTagLocation {
                        TypeID = classRef.DefID,
                        MethodIndex = methodInfo.Index,
                    },
                    StructType(var structRef) => new MethodTagLocation {
                        TypeID = structRef.DefID,
                        MethodIndex = methodInfo.Index,
                    },
                    VariantType(var variantRef) => new MethodTagLocation {
                        TypeID = variantRef.DefID,
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

    public bool TryGetInterfaceDef(InterfaceID id, [NotNullWhen(true)] out InterfaceDef? result) {
        if (!this.Interfaces.TryGetValue(id, out var decl) || decl is not DefInterfaceDecl(var def)) {
            result = null;
            return false;
        }

        result = def;
        return true;
    }

    public bool TryGetInterfaceImpl(FunctionID functionID, out InterfaceMethodImplRef result) {
        foreach (var (implType, impls) in this.InterfaceImpls) {
            foreach (var (interfaceRef, interfaceImpl) in impls) {
                foreach (var (methodID, methodFuncID) in interfaceImpl.Methods) {
                    if (methodFuncID == functionID 
                        && this.TryGetInterfaceDef(interfaceRef.DefID, out var interfaceDef)
                        && interfaceDef.TryFindMethod(methodID, out var methodDef)
                    ) {
                        result = new InterfaceMethodImplRef {
                            Interface = interfaceRef,
                            ImplType = implType,
                            MethodName = methodDef.Name,
                        };
                    }
                }
            }
        }

        result = default;
        return false;
    }

    public bool GetInterfaceImpls(
        IType type,
        [NotNullWhen(true)] out IReadOnlyDictionary<InterfaceRef, InterfaceImpl>? result
    ) {
        if (!this.InterfaceImpls.TryGetValue(type, out var resultDict)) {
            result = null;
            return false;
        }

        result = resultDict;
        return true;
    }

    public bool FindFunction(FunctionID id, [NotNullWhen(true)] out FunctionInfo? functionInfo) {
        return this.Functions.TryGetValue(id, out functionInfo);
    }

    public bool FindClosureSig(TypeDefID closureStructID, [NotNullWhen(true)] out FunctionSig? sig) {
        foreach (var (closureSig, closureTypeIDs) in this.Closures) {
            if (closureTypeIDs.Contains(closureStructID)) {
                sig = closureSig;
                return true;
            }
        }

        sig = null;
        return false;
    }
}

[MessagePackObject]
public class TypeInfo {
    [Key("name")]
    public StringID? Name { get; init; }

    [Key("methods")]
    public required IReadOnlyList<MethodInfo> Methods {
        get;
        init => field = value.ToArrayNonNull();
    }
    
    [Key("flags")]
    public ulong Flags { get; init; }
}
