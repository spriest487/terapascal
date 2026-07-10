using System.Diagnostics.CodeAnalysis;
using System.Text;

namespace Terapascal.IR;

public interface IMetadataSource {
    private const int InstructionWidth = 8;

    bool FindVariable(VariableID id, [NotNullWhen(true)] out VariableInfo? variableInfo);

    bool FindVariantDef(TypeDefID id, [NotNullWhen(true)] out VariantDef? def);
    bool FindStructDef(TypeDefID id, [NotNullWhen(true)] out StructDef? def);

    bool FindStringLiteral(StringID id, [NotNullWhen(true)] out string? literal);

    IEnumerable<(TypeDefID ID, ITypeDef TypeDef)> GetTypeDefs();
    bool FindTypeDef(TypeDefID id, [NotNullWhen(true)] out ITypeDef? def);
    bool FindTypeDecl(TypeDefID id, [NotNullWhen(true)] out ITypeDecl? decl);
    bool IsTypeDefined(IType type);

    IEnumerable<(ITagLocation, IReadOnlyList<TagInfo>)> GetAllTags();

    IEnumerable<(InterfaceID ID, InterfaceDef InterfaceDef)> GetInterfaceDefs();
    bool FindInterfaceDecl(InterfaceID id, [NotNullWhen(true)] out IInterfaceDecl? interfaceDecl);

    bool TryGetInterfaceDef(InterfaceID id, [NotNullWhen(true)] out InterfaceDef? def);

    bool TryGetInterfaceImpl(FunctionID functionID, out InterfaceMethodImplRef result);
    bool GetInterfaceImpls(
        IType type,
        [NotNullWhen(true)] out IReadOnlyDictionary<InterfaceRef, InterfaceImpl>? result
    );

    bool FindFunction(FunctionID id, [NotNullWhen(true)] out FunctionInfo? functionInfo);

    bool FindClosureSig(TypeDefID closureStructID, [NotNullWhen(true)] out FunctionSig? sig);

    void FormatInstruction(IInstruction instruction, StringBuilder result) {
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
                break;
                ;
            }

            case OrInstruction(var op): {
                this.FormatInstruction(op, "or", "or", result);
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

                result.Append('(');
                this.FormatRef(callInstruction.SelfArg, result);

                var ifaceTypeDisplay = callInstruction.InterfaceRef.ToObjectID().ToString(this);
                result.Append($" as {ifaceTypeDisplay}).");

                this.FormatInterfaceMethod(callInstruction.InterfaceRef, callInstruction.MethodID, result);

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
                result.Append($" as {castInstruction.Type.ToString(this)}");
                break;
            }

            case IsTypeInstruction classIsInstruction: {
                this.FormatInstructionPrefix("is", result);

                this.FormatOutputRef(classIsInstruction.Out, result);
                this.FormatValue(classIsInstruction.Arg, result);
                result.Append($" is {classIsInstruction.IsType.ToString(this)}");
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
                result.Append(localAllocInstruction.Type.ToString(this));
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

                result.Append($"[{newArrayInstruction.ElementType.ToString(this)}, ");
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
                result.Append($"[{newBoxInstruction.ElementType.ToString(this)}]");

                if (newBoxInstruction.Immortal) {
                    result.Append(" (immortal)");
                }

                break;
            }

            case NewObjectInstruction newInstruction: {
                this.FormatInstructionPrefix("new", result);

                this.FormatOutputRef(newInstruction.Out, result);

                var objectType = newInstruction.TypeID.ToObjectType(newInstruction.TypeArgs ?? []);
                result.Append($" new {objectType.ToString(this)}");

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

    private void FormatInterfaceMethod(InterfaceRef ifaceRef, MethodID methodID, StringBuilder result) {
        if (!this.TryGetInterfaceDef(ifaceRef.DefID, out var interfaceDef)
            || !interfaceDef.TryFindMethod(methodID, out var methodDef)
        ) {
            result.Append($"<method {methodID}>");
            return;
        }

        result.Append(methodDef.Name);
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

    void FormatValue(IValue v, StringBuilder result) {
        switch (v) {
            case DefaultValue(var ty): result.Append($"default({ty.ToString(this)})"); break;

            case SizeOfValue(var ty): result.Append($"sizeof({ty.ToString(this)})"); break;

            case LiteralNilValue: result.Append("NIL"); break;

            case LiteralValue<bool>(var lit): result.Append(lit ? "true" : "false"); break;

            case LiteralValue<byte>(var lit): result.Append(lit); break;

            case LiteralValue<sbyte>(var lit): result.Append(lit); break;

            case LiteralValue<ushort>(var lit): result.Append(lit); break;

            case LiteralValue<short>(var lit): result.Append(lit); break;

            case LiteralValue<uint>(var lit): result.Append(lit); break;

            case LiteralValue<int>(var lit): result.Append(lit); break;

            case LiteralValue<ulong>(var lit): result.Append(lit); break;

            case LiteralValue<long>(var lit): result.Append(lit); break;

            case LiteralValue<nuint>(var lit): result.Append(lit); break;

            case LiteralValue<nint>(var lit): result.Append(lit); break;

            case LiteralValue<float>(var lit): result.Append(lit); break;

            case LiteralValue<double>(var lit): result.Append(lit); break;

            case RefValue(var r): this.FormatRef(r, result); break;

            default: result.Append(v); break;
        }
    }

    void FormatRef(IRef r, StringBuilder result) {
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

                var structTypeRef = field.InstanceType.GetTypeRef();
                if (structTypeRef != null && this.FindStructDef(structTypeRef.DefID, out var structDef)) {
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
                var typeDefID = varData.InstanceType.GetTypeRef();
                if (typeDefID != null
                    && this.FindVariantDef(typeDefID.DefID, out var variantDef)
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

            case GlobalRef(FunctionGlobalRef(var funcRef)): {
                this.FormatFunctionRef(funcRef, result);
                break;
            }

            case GlobalRef(VariableGlobalRef(var id)): {
                if (this.FindVariable(id, out var variableInfo) && variableInfo.Name != null) {
                    result.Append(variableInfo.Name);
                } else {
                    result.Append(id.ToString());
                }

                break;
            }

            case GlobalRef(StaticFuncInfoGlobalRef(var funcID)): {
                result.Append("funcinfo(");
                this.FormatFunctionRef(funcID.ToFunctionRef([]), result);
                result.Append(')');
                break;
            }

            case GlobalRef(StaticTypeInfoGlobalRef(var ty)): {
                result.Append("typeinfo(");
                result.Append(ty.ToString(this));
                result.Append(')');
                break;
            }

            case GlobalRef(StringLiteralGlobalRef(var id)): {
                if (this.FindStringLiteral(id, out var text)) {
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

    internal void FormatFunctionRef(FunctionRef funcRef, StringBuilder result) {
        this.FormatFunctionName(funcRef.DefID, result);

        NamePath.FormatTypeArgsList(funcRef.TypeArgs, this, result);
    }

    internal void FormatFunctionName(FunctionID id, StringBuilder result) {
        string? funcName = null;
        if (this.FindFunction(id, out var funcInfo)) {
            funcName = funcInfo.Identity.ToString(this);
        }

        if (funcName == null && this.TryGetInterfaceImpl(id, out var methodImplRef)) {
            var interfaceName = methodImplRef.Interface.ToObjectID().ToString(this);
            var implTypeName = methodImplRef.ImplType.ToString(this);

            funcName = $"{interfaceName} ({implTypeName}).{methodImplRef.MethodName}";
        }

        funcName ??= id.ToString();
        result.Append(funcName);
    }

    bool FindDeclPath(IType type, [NotNullWhen(true)] out DeclPath? path) {
        switch (type) {
            case StructType(var typeRef): return this.FindDeclPath(typeRef, out path);
            case VariantType(var typeRef): return this.FindDeclPath(typeRef, out path);

            case ObjectType(var objectID): return this.FindDeclPath(objectID, out path);
            case WeakObjectType(var objectID): return this.FindDeclPath(objectID, out path);

            default: {
                path = null;
                return false;
            }
        }
    }

    bool FindDeclPath(TypeRef typeRef, [NotNullWhen(true)] out DeclPath? path) {
        path = null;

        if (!this.FindTypeDecl(typeRef.DefID, out var decl)) {
            return false;
        }

        switch (decl) {
            case DefTypeDecl(StructTypeDef(var structDef)): {
                path = structDef.Identity.GetDeclPath();
                return path != null;
            }

            case DefTypeDecl(VariantTypeDef(var variantDef)): {
                path = variantDef.Name;
                return true;
            };

            case ForwardTypeDecl(var forwardPath): {
                path = forwardPath;
                return true;
            }

            case ReservedTypeDecl: {
                return false;
            }

            default: {
                throw new ArgumentOutOfRangeException(nameof(decl));
            }
        }
    }

    bool FindDeclPath(IObjectID objectID, [NotNullWhen(true)] out DeclPath? path) {
        path = null;

        switch (objectID) {

            case ClassObjectID(var classRef): {
                return this.FindDeclPath(classRef, out path);
            }
            case InterfaceObjectID(var ifaceRef): {
                if (!this.FindInterfaceDecl(ifaceRef.DefID, out var ifaceDecl)) {
                    return false;
                }

                path = ifaceDecl.GetDeclName();
                return true;
            }

            case AnyClosureObjectID:
            case AnyObjectID:
            case ArrayObjectID:
            case BoxObjectID: {
                return false;
            }

            default: throw new ArgumentOutOfRangeException(nameof(objectID));
        }
    }
}
