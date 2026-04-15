using System.Diagnostics.CodeAnalysis;
using System.Text;
using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class Metadata {
    private const int InstructionWidth = 8;
    
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
            foreach (var (interfaceID, interfaceImpl) in impls) {
                foreach (var (methodID, methodFuncID) in interfaceImpl.Methods) {
                    if (methodFuncID == functionID 
                        && this.TryGetInterfaceDef(interfaceID, out var interfaceDef)
                        && interfaceDef.TryFindMethod(methodID, out var methodDef)
                    ) {
                        result = new InterfaceMethodImplRef {
                            Interface = interfaceDef.Name,
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

    public void FormatInstruction(IInstruction instruction, StringBuilder result) {
        switch (instruction) {
            case BitAndInstruction(var op): {
                this.FormatInstruction(op, "bitand", "&", result);
                break;
            }
            case BitOrInstruction(var op): {
                this.FormatInstruction(op, "bitor", "|", result);
                break;
            }
            case BitXorInstruction(var op): {
                this.FormatInstruction(op, "xor", "^", result);
                break;
            }
            case ShlInstruction(var op): {
                this.FormatInstruction(op, "shl", "shl", result);
                break;
            }
            case ShrInstruction(var op): {
                this.FormatInstruction(op, "shr", "shr", result);
                break;
            }

            case LtInstruction(var op): {
                this.FormatInstruction(op, "lt", "lt", result);
                break;
            }
            case LteInstruction(var op): {
                this.FormatInstruction(op, "lte", "lte", result);
                break;
            }
            case GtInstruction(var op): {
                this.FormatInstruction(op, "gt", "gt", result);
                break;
            }
            case GteInstruction(var op): {
                this.FormatInstruction(op, "gte", "gte", result);
                break;
            }

            case MulInstruction(var op): {
                this.FormatInstruction(op, "mul", "*", result);
                break;
            }
            case FDivInstruction(var op): {
                this.FormatInstruction(op, "fdiv", "/", result);
                break;
            }
            case IDivInstruction(var op): {
                this.FormatInstruction(op, "div", "div", result);
                break;
            }
            case ModInstruction(var op): {
                this.FormatInstruction(op, "xor", "mod", result);
                break;
            }

            case AddInstruction(var op): {
                this.FormatInstruction(op, "add", "+", result);
                break;
            }
            case SubInstruction(var op): {
                this.FormatInstruction(op, "sub", "-", result);
                break;
            }

            case EqInstruction(var op): {
                this.FormatInstruction(op, "eq", "=", result);
                break;
            }

            case AndInstruction(var op): {
                this.FormatInstruction(op, "and", "and", result);
                break;;
            }

            case OrInstruction(var op): {
                this.FormatInstruction(op, "or", "or",result);
                break;
            }

            case BitNotInstruction(var op): {
                this.FormatInstruction(op, "bitnot", "~", result);
                break;
            }
            case NotInstruction(var op): {
                this.FormatInstruction(op, "not", "not", result);
                break;
            }

            case AddrOfInstruction addrOf: {
                this.FormatInstructionPrefix("addrof", result);

                this.FormatOutputRef(addrOf.Out, result);
                result.Append('@');
                this.FormatRef(addrOf.Arg, result);
                break;
            }

            case CallInstruction callInstruction: {
                this.FormatInstructionPrefix("call", result);

                if (callInstruction.Out != null) {
                    this.FormatOutputRef(callInstruction.Out, result);
                }
                
                this.FormatValue(callInstruction.Function, result);
                result.Append('(');
                for (var i = 0; i < callInstruction.Args.Count; i += 1) {
                    if (i > 0) {
                        result.Append(", ");
                    }
                    this.FormatValue(callInstruction.Args[i], result);
                }

                result.Append(')');
                break;
            }

            case VirtualCallInstruction callInstruction: {
                this.FormatInstructionPrefix("vcall", result);

                if (callInstruction.Out != null) {
                    this.FormatOutputRef(callInstruction.Out, result);
                }

                if (this.TryGetInterfaceDef(callInstruction.InterfaceID, out var interfaceDef)
                    && interfaceDef.TryFindMethod(callInstruction.MethodID, out var methodDef)
                   ) {
                    this.FormatValue(callInstruction.SelfArg, result);
                    result.Append('.');
                    result.Append(methodDef.Name);
                } else {
                    var interfaceType = callInstruction.InterfaceID.InterfacePointerType();
                    var interfaceName = interfaceType.ToPrettyString(this);

                    result.Append('(');
                    this.FormatValue(callInstruction.SelfArg, result);
                    result.Append($" as {interfaceName}).{callInstruction.MethodID.ID}");
                }

                result.Append('(');
                if (callInstruction.RestArgs != null) {
                    for (var i = 0; i < callInstruction.RestArgs.Count; i += 1) {
                        if (i > 0) {
                            result.Append(", ");
                        }

                        this.FormatValue(callInstruction.RestArgs[i], result);
                    }
                }

                result.Append(')');
                break;
            }

            case CastInstruction castInstruction: {
                this.FormatInstructionPrefix("cast", result);

                this.FormatOutputRef(castInstruction.Out, result);
                this.FormatValue(castInstruction.Value, result);
                result.Append($" as {castInstruction.Type.ToPrettyString(this)}");
                break;
            }

            case ClassIsInstruction classIsInstruction: {
                this.FormatInstructionPrefix("is", result);

                this.FormatOutputRef(classIsInstruction.Out, result);
                this.FormatValue(classIsInstruction.Arg, result);
                result.Append($" is {classIsInstruction.ClassID.ToPrettyString(this)}");
                break;
            }

            case CommentInstruction commentInstruction: {
                result.Append($"// {commentInstruction.Text}");
                break;
            }

            case JumpInstruction jmp: {
                this.FormatInstructionPrefix("jmp", result);
                result.Append(jmp.Destination);
                break;
            }
            case JumpIfInstruction jmpIf: {
                this.FormatInstructionPrefix("jmpif", result);

                result.Append(jmpIf.Destination);

                result.Append(" if ");
                this.FormatValue(jmpIf.Condition, result);
                break;
            }

            case LabelInstruction labelInstruction: {
                result.Append($"{labelInstruction.Label}");
                break;
            }

            case LengthInstruction lengthInstruction: {
                this.FormatInstructionPrefix("length", result);
                this.FormatOutputRef(lengthInstruction.Out, result);
                
                this.FormatRef(lengthInstruction.Arg, result);

                result.Append(".length");
                break;
            }

            case MakeRefInstruction makeRefInstruction: {
                this.FormatInstructionPrefix("makeref", result);
                this.FormatOutputRef(makeRefInstruction.Out, result);

                result.Append('&');
                this.FormatRef(makeRefInstruction.Arg, result);
                break;
            }

            case LocalAllocInstruction localAllocInstruction: {
                this.FormatInstructionPrefix("local", result);
                result.Append(localAllocInstruction.At.ToString());
                result.Append(" of ");
                result.Append(localAllocInstruction.Type.ToPrettyString(this));
                break;
            }

            case MoveInstruction moveInstruction: {
                this.FormatInstructionPrefix("mov", result);
                this.FormatOutputRef(moveInstruction.Out, result);
                this.FormatValue(moveInstruction.NewValue, result);
                break;
            }

            case NewArrayInstruction newArrayInstruction: {
                this.FormatInstructionPrefix("newarr", result);

                this.FormatOutputRef(newArrayInstruction.Out, result);
                
                result.Append($"[{newArrayInstruction.ElementType.ToPrettyString(this)}, ");
                this.FormatValue(newArrayInstruction.Count, result);
                result.Append(']');
                
                if (newArrayInstruction.Immortal) {
                    result.Append(" (immortal)");
                }
                
                break;
            }

            case NewBoxInstruction newBoxInstruction: {
                this.FormatInstructionPrefix("newbox", result);
                
                this.FormatOutputRef(newBoxInstruction.Out, result);
                result.Append($"[{newBoxInstruction.ElementType.ToPrettyString(this)}]");
                
                if (newBoxInstruction.Immortal) {
                    result.Append(" (immortal)");
                }
                
                break;
            }

            case NewInstruction newInstruction: {
                this.FormatInstructionPrefix("new", result);

                this.FormatOutputRef(newInstruction.Out, result);
                result.Append($" new {newInstruction.TypeID.ToObjectType().ToPrettyString(this)}");
                
                if (newInstruction.Immortal) {
                    result.Append(" (immortal)");
                }

                break;
            }

            case RaiseInstruction raiseInstruction: {
                this.FormatInstructionPrefix("raise", result);
                this.FormatRef(raiseInstruction.Value, result);
                break;
            }

            case ReleaseInstruction releaseInstruction: {
                this.FormatInstructionPrefix("release", result);
                if (releaseInstruction.ReleasedOut != null) {
                    this.FormatOutputRef(releaseInstruction.ReleasedOut, result);
                }

                result.Append("--");
                result.Append(releaseInstruction.Weak ? "weak" : "strong");
                result.Append(' ');
                
                this.FormatRef(releaseInstruction.At, result);

                break;
            }

            case RetainInstruction retainInstruction: {
                this.FormatInstructionPrefix("retain", result);

                result.Append("++");
                result.Append(retainInstruction.Weak ? "weak" : "strong");
                result.Append(' ');

                this.FormatRef(retainInstruction.At, result);
                break;
            }

            default: {
                result.Append('?');
                break;
            }
        }
    }

    private void FormatOutputRef(IRef outRef, StringBuilder result) {
        this.FormatRef(outRef, result);
        result.Append(" := ");
    }

    private void FormatInstruction(BinOpInstruction instruction, string name, string op, StringBuilder result) {
        this.FormatInstructionPrefix(name, result);
        
        this.FormatOutputRef(instruction.Out, result);

        this.FormatValue(instruction.ArgA, result);
        result.Append($" {op} ");
        this.FormatValue(instruction.ArgB, result);
    }
    
    private void FormatInstruction(UnaryOpInstruction instruction, string name, string op, StringBuilder result) {
        this.FormatInstructionPrefix(name, result);
        
        this.FormatOutputRef(instruction.Out, result);

        result.Append($"{op} ");
        this.FormatValue(instruction.Arg, result);
    }

    private void FormatInstructionPrefix(string name, StringBuilder result) {
        result.Append(name.PadLeft(InstructionWidth));
        result.Append(' ');
    }
    
    public void FormatValue(IValue v, StringBuilder result) {
        switch (v) {
            case DefaultValue(var ty):
                result.Append($"default({ty.ToPrettyString(this)})");
                break;

            case SizeOfValue(var ty):
                result.Append($"sizeof({ty.ToPrettyString(this)})");
                break;
            
            case LiteralNilValue: 
                result.Append("NIL");
                break;

            case LiteralValue<bool>(var lit):
                result.Append(lit ? "true" : "false");
                break;
            
            case LiteralValue<byte>(var lit):
                result.Append(lit);
                break;
            
            case LiteralValue<sbyte>(var lit):
                result.Append(lit);
                break;

            case LiteralValue<ushort>(var lit):
                result.Append(lit);
                break;
            
            case LiteralValue<short>(var lit):
                result.Append(lit);
                break;
            
            case LiteralValue<uint>(var lit):
                result.Append(lit);
                break;
            
            case LiteralValue<int>(var lit):
                result.Append(lit);
                break;
            
            case LiteralValue<ulong>(var lit):
                result.Append(lit);
                break;
            
            case LiteralValue<long>(var lit):
                result.Append(lit);
                break;
            
            case LiteralValue<nuint>(var lit):
                result.Append(lit);
                break;
            
            case LiteralValue<nint>(var lit):
                result.Append(lit);
                break;
            
            case LiteralValue<float>(var lit):
                result.Append(lit);
                break;
            
            case LiteralValue<double>(var lit):
                result.Append(lit);
                break;

            case RefValue(var r):
                this.FormatRef(r, result);
                break;

            default: 
                result.Append(v);
                break;
        }
    }

    public void FormatRef(IRef r, StringBuilder result) {
        switch (r) {
            case ArgRef(var id): {
                result.Append(id.ToString());
                break;
            }

            case Deref(var val): {
                this.FormatValue(val, result);
                result.Append('^');
                break;
            }

            case ElementRef(var element): {
                result.Append("&(");
                this.FormatRef(element.Instance, result);
                result.Append(")[");
                this.FormatValue(element.Index, result);
                result.Append(']');
                break;
            }

            case FieldRef(var field): {
                string? fieldName = null;

                var structID = field.InstanceType.GetTypeDefID();
                if (structID != null && this.FindStructDef(structID.Value, out var structDef)) {
                    if (structDef.Fields.TryGetValue(field.FieldID, out var fieldDef)) {
                        fieldName = fieldDef.Name;
                    }
                }

                fieldName ??= $"{field.FieldID.ID}";
                
                result.Append("&(");
                this.FormatRef(field.Instance, result);
                result.Append($").{fieldName}");
                break;
            }

            case VariantDataRef(var varData): {
                string? caseName = null;
                var typeDefID = varData.InstanceType.GetTypeDefID();
                if (typeDefID != null
                    && this.FindVariantDef(typeDefID.Value, out var variantDef)
                    && varData.CaseIndex < (ulong)variantDef.Cases.Count
                ) {
                    caseName = variantDef.Cases[(int)varData.CaseIndex].Name;
                }

                caseName ??= $"{varData.CaseIndex}";

                result.Append("&(");
                this.FormatRef(varData.Instance, result);
                result.Append($").{caseName}");
                break;
            }

            case VariantTagRef(var varTag): {
                result.Append("&(tag of ");
                this.FormatRef(varTag.Instance, result);
                result.Append(')');
                break;
            }

            case GlobalRef(FunctionGlobalRef(var id)): {
                this.FormatFunctionName(id, result);
                break;
            }

            case GlobalRef(VariableGlobalRef(var id)): {
                if (this.Variables.TryGetValue(id, out var variableInfo) && variableInfo.Name != null) {
                    result.Append(variableInfo.Name.ToPrettyString(this));
                } else {
                    result.Append(id.ToString());
                }
                break;
            }

            case GlobalRef(StaticFuncInfoGlobalRef(var funcID)): {
                result.Append("funcinfo(");
                this.FormatFunctionName(funcID, result);
                result.Append(')');
                break;
            }

            case GlobalRef(StaticTypeInfoGlobalRef(var ty)): {
                result.Append("typeinfo(");
                result.Append(ty.ToPrettyString(this));
                result.Append(')');
                break;
            }

            case GlobalRef(StringLiteralGlobalRef(var id)): {
                if (this.StringLiterals.TryGetValue(id, out var text)) {
                    result.Append($"'{text}'");
                } else {
                    result.Append(id.ToString());
                }
                break;
            }

            case LocalRef(var id): {
                result.Append(id.ToString());
                break;
            }

            default: {
                result.Append(r);
                break;
            }
        }
    }

    internal void FormatFunctionName(FunctionID id, StringBuilder result) {
        string? funcName = null;
        if (this.Functions.TryGetValue(id, out var funcInfo)) {
            funcName = funcInfo.GlobalName?.ToPrettyString(this);
        }

        if (funcName == null && this.TryGetInterfaceImpl(id, out var methodImplRef)) {
            var interfaceName = methodImplRef.Interface.ToPrettyString(this);
            var implTypeName = methodImplRef.ImplType.ToPrettyString(this);
                    
            funcName = $"{interfaceName} ({implTypeName}).{methodImplRef.MethodName}";
        }
                
        funcName ??= id.ToString();
        result.Append(funcName);
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
    public NamePath? Name { get; init; }
    
    [Key("type")]
    public required IType Type { get; init; }
}
