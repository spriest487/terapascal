using System.Text;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

namespace Terapascal.CIL;

internal readonly record struct LocalMapping(IR.IType Type, int VariableIndex);

public class InstructionBuilder {
    private readonly IR.Library library;

    private readonly AssemblyBuilder assemblyBuilder;

    private readonly MethodDefinition method;

    private readonly Dictionary<IR.LocalID, LocalMapping> localMappings;
    private readonly Dictionary<IR.ArgID, LocalMapping> argMappings;
    private LocalMapping? resultMapping;

    private readonly ILProcessor body;

    private readonly Dictionary<IR.Label, Instruction> labelInstructions;
    private readonly Dictionary<IR.Label, List<Instruction>> unresolvedJmps;

    private MethodReference? readStringMethod;
    private MethodReference? rcRetainMethod;
    private MethodReference? rcReleaseMethod;

    private bool HasReturnValue => !this.method.ReturnType.Equals(this.assemblyBuilder.TypeSystem.Void);

    public InstructionBuilder(
        AssemblyBuilder assemblyBuilder,
        IR.Library library,
        MethodDefinition method
    ) {
        this.library = library;
        this.method = method;
        this.assemblyBuilder = assemblyBuilder;

        this.localMappings = new Dictionary<IR.LocalID, LocalMapping>();
        this.argMappings = new Dictionary<IR.ArgID, LocalMapping>();
        this.resultMapping = null;
        
        this.body = this.method.Body.GetILProcessor();

        this.labelInstructions = new Dictionary<IR.Label, Instruction>();
        this.unresolvedJmps = new Dictionary<IR.Label, List<Instruction>>();
    }

    public void BeginFunction(IR.FunctionDef function) {
        this.method.Body.InitLocals = true;

        var returnType = function.Signature.ResultType;

        // create a variable to hold the result variable %0 if there's a return type
        if (this.HasReturnValue) {
            var returnVar = new VariableDefinition(this.method.ReturnType);
            this.method.Body.Variables.Add(returnVar);

            this.resultMapping = new LocalMapping(returnType, returnVar.Index);
        }

        var paramTypes = function.Signature.ParameterTypes;

        // params don't need local vars, we can reference them with ldarg
        for (var i = 0; i < paramTypes.Count; i += 1) {
            var paramLocal = new IR.ArgID((ulong)i);
            var paramType = paramTypes[i];

            var mapping = new LocalMapping(paramType, i);
            this.argMappings.Add(paramLocal, mapping);
        }
    }

    public void AddInstructions(IReadOnlyList<IR.IInstruction> instructions) {
        var typeBuilder = this.assemblyBuilder.TypeBuilder;

        for (var pc = 0; pc < instructions.Count; pc += 1) {
            var instruction = instructions[pc];

            switch (instruction) {
                case IR.CommentInstruction: {
                    // ignored
                    break;
                }

                case IR.RetainInstruction { At: var atRef, Weak: var weak }: {
                    const string methodName = nameof(Runtime.SystemFunctions.RcRetain);
                    this.rcRetainMethod ??= this.assemblyBuilder.FindRuntimeFunction(methodName);
                    
                    this.LoadRef(atRef);
                    this.body.Emit(weak ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
                    
                    this.body.Emit(OpCodes.Call, this.rcRetainMethod);
                    break;
                }

                case IR.ReleaseInstruction { At: var atRef, Weak: var weak, ReleasedOut: var outRef }: {
                    const string methodName = nameof(Runtime.SystemFunctions.RcRelease);
                    this.rcReleaseMethod ??= this.assemblyBuilder.FindRuntimeFunction(methodName);
                    
                    this.StoreRef(outRef, () => {
                        this.LoadRef(atRef);
                        this.body.Emit(weak ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);

                        this.body.Emit(OpCodes.Call, this.rcReleaseMethod);
                        
                        // if the result is true (destroyed), always reset the pointer to null
                        var aliveLabel = Instruction.Create(OpCodes.Nop);
                        
                        this.body.Emit(OpCodes.Dup);
                        this.body.Emit(OpCodes.Brfalse, aliveLabel);

                        this.StoreRef(atRef, () => {
                            this.body.Emit(OpCodes.Ldnull);
                        });
                        
                        this.body.Append(aliveLabel);
                    });
                    
                    break;
                }

                case IR.MoveInstruction { Out: var outRef, NewValue: var newVal }: {
                    this.StoreRef(outRef, () => {
                        this.LoadValue(newVal);
                    });
                    break;
                }

                case IR.LengthInstruction {
                    Out: var outRef,
                    Arg: var argRef,
                    ArrayType: var arrayType,
                }: {
                    this.StoreRef(outRef, () => {
                        if (arrayType is IR.ArrayType staticArrayType) {
                            this.body.Emit(OpCodes.Ldc_I4, (int)staticArrayType.Length);
                        } else if (arrayType is IR.ObjectType) {
                            // must be a dynarray
                            this.LoadRef(argRef);
                            this.body.Emit(OpCodes.Ldlen);
                        } else {
                            this.body.Emit(OpCodes.Ldc_I4, 1);
                        }
                    });
                    
                    break;
                }

                case IR.NewObjectInstruction {
                    Out: var outRef,
                    TypeID: var typeID,
                    TypeArgs: var typeArgs,
                    Immortal: var immortal,
                }: {
                    this.BuildNewObject(outRef, typeID.ToTypeRef(typeArgs), immortal);

                    break;
                }

                case IR.NewArrayInstruction {
                    Out: var outRef,
                    ElementType: var elementType,
                    Count: var countVal,
                    Immortal: var immortal,
                }: {
                    this.BuildNewArray(outRef, countVal, elementType, immortal);
                    break;
                }

                case IR.NewBoxInstruction {
                    Out: var outRef,
                    ElementType: var elementType,
                    Immortal: var immortal,
                }: {
                    this.BuildNewBox(outRef, elementType, immortal);
                    break;
                }

                case IR.CastInstruction {
                    Out: var outRef,
                    Value: var val,
                    Type: var castToType,
                }: {
                    this.StoreRef(outRef, () => {
                        this.LoadValue(val);

                        switch (castToType) {
                            case IR.I8Type: {
                                this.body.Emit(OpCodes.Conv_I1);
                                break;
                            }
                            case IR.BoolType:
                            case IR.U8Type: {
                                this.body.Emit(OpCodes.Conv_U1);
                                break;
                            }
                            case IR.I16Type: {
                                this.body.Emit(OpCodes.Conv_I2);
                                break;
                            }
                            case IR.U16Type: {
                                this.body.Emit(OpCodes.Conv_U2);
                                break;
                            }
                            case IR.I32Type: {
                                this.body.Emit(OpCodes.Conv_I4);
                                break;
                            }
                            case IR.U32Type: {
                                this.body.Emit(OpCodes.Conv_U4);
                                break;
                            }
                            case IR.I64Type: {
                                this.body.Emit(OpCodes.Conv_I8);
                                break;
                            }
                            case IR.U64Type: {
                                this.body.Emit(OpCodes.Conv_U8);
                                break;
                            }
                            case IR.TempRefType:
                            case IR.PointerType:
                            case IR.ISizeType: {
                                this.body.Emit(OpCodes.Conv_I);
                                break;
                            }
                            case IR.USizeType: {
                                this.body.Emit(OpCodes.Conv_U);
                                break;
                            }

                            case IR.F32Type: {
                                this.body.Emit(OpCodes.Conv_R4);
                                break;
                            }
                            case IR.F64Type: {
                                this.body.Emit(OpCodes.Conv_R8);
                                break;
                            }

                            case IR.ObjectType:
                            case IR.WeakObjectType: {
                                var typeRef = typeBuilder.BuildType(castToType);
                                this.body.Emit(OpCodes.Castclass, typeRef);
                                break;
                            }

                            case IR.NothingType:
                            case IR.FunctionType:
                            case IR.StructType:
                            case IR.VariantType:
                            case IR.ArrayType: {
                                // no conversion, typechecking should prevent casts like this
                                break;
                            }
                            
                            default: {
                                throw new NotImplementedException($"conversion to {castToType.ToString(this.library.Metadata)}");
                            }
                        }
                    });
                    break;
                }

                case IR.IsTypeInstruction {
                    Out: var outRef,
                    Arg: var argVal,
                    ValueType: var valueType,
                }: {
                    var valueTypeRef = typeBuilder.BuildType(valueType);
                    
                    var isMethodRef = this.assemblyBuilder.TypeBuilder.ObjectIsMethod;

                    var isMethodInstance = new GenericInstanceMethod(isMethodRef);
                    isMethodInstance.GenericArguments.Add(valueTypeRef);

                    var isMethodInstanceRef = this.assemblyBuilder.Module.ImportReference(isMethodInstance);

                    this.StoreRef(outRef, () => {
                        this.LoadValue(argVal);
                        this.body.Emit(OpCodes.Call, isMethodInstanceRef);
                    });

                    break;
                }

                case IR.RaiseInstruction { Value: var val }: {
                    this.BuildRaise(val);
                    break;
                }

                case IR.AddrOfInstruction {
                    Out: var outRef,
                    Arg: var argRef,
                }: {
                    this.StoreRef(outRef, () => {
                        this.LoadRefAddr(argRef);
                    });
                    break;
                }
                
                case IR.MakeRefInstruction {
                    Out: var outRef,
                    Arg: var argRef,
                }: {
                    this.StoreRef(outRef, () => {
                        this.LoadRefAddr(argRef);
                    });
                    break;
                }

                case IR.EqInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Ceq);
                    break;
                }

                case IR.GtInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Cgt);
                    break;
                }

                case IR.GteInstruction(var op): {
                    this.BuildCompareOrEqualOperation(op, OpCodes.Clt);
                    break;
                }

                case IR.LtInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Clt);
                    break;
                }

                case IR.LteInstruction(var op): {
                    this.BuildCompareOrEqualOperation(op, OpCodes.Cgt);
                    break;
                }

                case IR.AndInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.And);
                    break;
                }

                case IR.OrInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Or);
                    break;
                }

                case IR.NotInstruction(var op): {
                    this.StoreRef(op.Out, () => {
                        this.LoadValue(op.Arg);
                        this.body.Emit(OpCodes.Ldc_I4_0);
                        this.body.Emit(OpCodes.Ceq);    
                    });
                    break;
                }

                case IR.BitNotInstruction(var op): {
                    this.StoreRef(op.Out, () => {
                        this.LoadValue(op.Arg);
                        this.body.Emit(OpCodes.Not);
                    });
                    break;
                }
                
                case IR.BitOrInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Or);
                    break;
                }

                case IR.BitXorInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Xor);
                    break;
                }

                case IR.BitAndInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.And);
                    break;
                }

                case IR.ShlInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Shl);
                    break;
                }

                case IR.ShrInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Shr);
                    break;
                }

                case IR.AddInstruction(var op): {
                    this.BuildAdd(op);
                    break;
                }

                case IR.SubInstruction(var op): {
                    this.BuildSubtract(op);
                    break;
                }

                case IR.MulInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Mul);
                    break;
                }

                case IR.IDivInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Div);
                    break;
                }

                case IR.FDivInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Div);
                    break;
                }

                case IR.LocalAllocInstruction(var at, var type): {
                    this.BuildLocalAlloc(at, type);

                    break;
                }

                case IR.CallInstruction {
                    Out: var outRef,
                    Args: var argVals,
                    Function: var funcVal,
                }: {
                    this.BuildCall(outRef, funcVal, argVals);

                    break;
                }

                case IR.VirtualCallInstruction {
                    Out: var outRef,
                    SelfArg: var selfArg,
                    RestArgs: var restArgs,
                    InterfaceRef: var interfaceRef,
                    MethodID: var methodID,
                }: {
                    this.BuildVirtualCall(outRef, selfArg, restArgs, interfaceRef, methodID);
                    break;
                }

                case IR.JumpInstruction { Destination: var dest }: {
                    this.InsertJmp(dest, OpCodes.Br);
                    break;
                }
                
                case IR.JumpIfInstruction { Destination: var dest, Condition: var condVal }: {
                    this.LoadValue(condVal);
                    this.InsertJmp(dest, OpCodes.Brtrue);
                    break;
                }

                case IR.LabelInstruction { Label: var label }: {
                    this.InsertLabel(label);
                    break;
                }

                default: {
                    Console.Error.WriteLine($"skipping unimplemented instruction type: {instruction.GetType()}");
                    break;
                }
            }
        }
    }

    private void LoadFieldRef(IR.IRef argRef, IR.FieldID fieldID, IR.IType baseType) {
        var typeBuilder = this.assemblyBuilder.TypeBuilder;
                    
        var fieldRef = typeBuilder.GetFieldRef(baseType, fieldID);

        // for object types Ldfld(a) expects an object ref on the stack so load it directly, but for
        // value types we need to load a reference (address) to the object
        if (baseType.IsObjectType()) {
            this.LoadRef(argRef);
        } else {
            this.LoadRefAddr(argRef);
        }

        this.body.Emit(OpCodes.Ldflda, fieldRef.Field);
    }

    private void LoadElementRef(IR.IRef baseRef, IR.IValue indexVal, IR.IType baseType) {
        var typeBuilder = this.assemblyBuilder.TypeBuilder;
        
        switch (baseType) {
            case IR.ArrayType { Element: var elementType, Length: var length }: {
                this.LoadRefAddr(baseRef);
                this.LoadValue(indexVal);
                        
                var elementMethod = typeBuilder.GetStaticArrayElementMethodRef(elementType, length);
                        
                this.body.Emit(OpCodes.Call, elementMethod);
                break;
            }

            case IR.ObjectType(IR.ArrayObjectID(var elementType)): {
                var elementTypeRef = typeBuilder.BuildType(elementType);

                this.LoadRef(baseRef);
                this.LoadValue(indexVal);
                this.body.Emit(OpCodes.Ldelema, elementTypeRef);
                break;
            }

            case IR.ObjectType(IR.BoxObjectID(var valueType)): {
                typeBuilder.BuildType(valueType, out var valueTypeID);
                var boxTypeInfo = typeBuilder.GetBoxTypeInfo(valueTypeID);

                this.LoadRef(baseRef);
                this.body.Emit(OpCodes.Ldflda, boxTypeInfo.ValueFieldRef);
                break;
            }

            default: {
                var msg = $"illegal base type for element instruction: {baseType}";
                throw new InvalidDataException(msg);
            }
        }
    }

    private void BuildAdd(IR.BinOpInstruction op) {
        this.body.Emit(OpCodes.Nop);
        this.body.Emit(OpCodes.Nop);
        this.body.Emit(OpCodes.Nop);
        
        var aType = this.GetValueType(op.ArgA);
        var bType = this.GetValueType(op.ArgB);

        this.StoreRef(op.Out, () => {
            // pointer addition: if either side is a pointer and the other is an integer, the integer is
            // an offset and the value to be added should be the integer multiplied by the deref type size
            if (aType.GetDerefType() is { } aValueType
                && bType.IsInteger()) {
                this.LoadValue(op.ArgA);

                this.LoadValue(op.ArgB);
                this.body.Emit(OpCodes.Conv_I);

                this.EmitSizeOf(aValueType);
                this.body.Emit(OpCodes.Mul);
            } else if (aType.IsInteger()
                && bType.GetDerefType() is { } bValueType) {
                this.LoadValue(op.ArgA);
                this.body.Emit(OpCodes.Conv_I);

                this.EmitSizeOf(bValueType);
                this.body.Emit(OpCodes.Mul);

                this.LoadValue(op.ArgB);
            } else {
                this.LoadValue(op.ArgA);
                this.LoadValue(op.ArgB);
            }

            this.body.Emit(OpCodes.Add);
        });
        
        this.body.Emit(OpCodes.Nop);
        this.body.Emit(OpCodes.Nop);
        this.body.Emit(OpCodes.Nop);
    }

    private void BuildSubtract(IR.BinOpInstruction op) {
        var aType = this.GetValueType(op.ArgA);
        var bType = this.GetValueType(op.ArgB);

        this.StoreRef(op.Out, () => {
            // pointer subtraction: if the lhs is a pointer and the rhs is an integer, the integer is
            // a negative offset and the value to be subtracted should be the integer multiplied
            // by the deref type size
            if (aType.GetDerefType() is { } aValueType
                && bType.IsInteger()) {
                this.LoadValue(op.ArgA);

                this.LoadValue(op.ArgB);
                this.body.Emit(OpCodes.Conv_I);

                this.EmitSizeOf(aValueType);
                this.body.Emit(OpCodes.Mul);
            } else {
                this.LoadValue(op.ArgA);
                this.LoadValue(op.ArgB);
            }

            this.body.Emit(OpCodes.Sub);
        });
    }

    private void BuildNewObject(IR.IRef outRef, IR.TypeRef typeRef, bool immortal) {
        var classID = new IR.ClassObjectID(typeRef);
        var createMethodInst = this.assemblyBuilder.TypeBuilder.GetObjectCreateMethod(classID);

        this.StoreRef(outRef, () => {
            this.body.Emit(immortal ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
            this.body.Emit(OpCodes.Call, createMethodInst);
        });
    }

    private void BuildNewArray(IR.IRef outRef, IR.IValue countVal, IR.IType elementType, bool immortal) {
        var createMethodInst = this.assemblyBuilder.TypeBuilder.GetArrayCreateMethod(elementType);
        
        this.StoreRef(outRef, () => {
            this.LoadValue(countVal);
            this.body.Emit(immortal ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
            this.body.Emit(OpCodes.Call, createMethodInst);
        });
    }
    
    private void BuildNewBox(IR.IRef outRef, IR.IType valueType, bool immortal) {
        var boxObjectID = new IR.BoxObjectID(valueType);
        var boxCreateMethod = this.assemblyBuilder.TypeBuilder.GetObjectCreateMethod(boxObjectID);

        this.StoreRef(outRef, () => {
            this.body.Emit(immortal ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
            this.body.Emit(OpCodes.Call, boxCreateMethod);
        });
    }

    private void BuildRaise(IR.IRef val) {
        var exceptionType = this.assemblyBuilder.TypeBuilder.ErrorType;
        var stringTypeName = this.assemblyBuilder.TypeBuilder.CLRStringType.FullName;
        var ctor = (MethodReference)exceptionType.Resolve()
            .GetConstructors()
            .Single(ctor => ctor.Parameters.Count == 1
                && ctor.Parameters[0].ParameterType.FullName == stringTypeName);
        ctor = this.assemblyBuilder.Module.ImportReference(ctor);
        
        // TODO: native strings
        // as an optimization, if the value is a string literal, we can supply that directly rather than
        // converting the pascal string back to a CLR one
        if (val is IR.GlobalRef(IR.StringLiteralGlobalRef(var stringID))) {
            var stringLit = this.library.Metadata.StringLiterals[stringID];
            this.body.Emit(OpCodes.Ldstr, stringLit);
        } else {
            this.LoadRef(val);

            // convert string
            this.readStringMethod ??= this.assemblyBuilder.FindRuntimeFunction(nameof(Runtime.SystemFunctions.ReadString));
            this.body.Emit(OpCodes.Call, this.readStringMethod);
        }

        this.body.Emit(OpCodes.Newobj, ctor);
        this.body.Emit(OpCodes.Throw);
    }

    private void EmitSizeOf(IR.IType type) {
        if (type.IsObjectType()) {
            this.body.Emit(OpCodes.Sizeof, this.assemblyBuilder.TypeSystem.IntPtr);
            return;
        }

        var intrinsicSize = type.IntrinsicSize();
        if (intrinsicSize == null) {
            var typeRef = this.assemblyBuilder.TypeBuilder.BuildType(type);
            this.body.Emit(OpCodes.Sizeof, typeRef);
        } else {
            this.body.Emit(OpCodes.Ldc_I4, intrinsicSize.Value);
        }
    }
    
    private void EmitDefault(IR.IType type) {
        if (type.IsObjectType() || type is IR.PointerType) {
            this.body.Emit(OpCodes.Ldnull);
        } else if (type.IsInteger()) {
            this.body.Emit(OpCodes.Ldc_I4_0);
        } else {
            var typeRef = this.assemblyBuilder.TypeBuilder.BuildType(type);
            var typeDef = this.assemblyBuilder.TypeBuilder.ResolveCore(typeRef) ?? typeRef.Resolve();

            if (typeDef?.FindConstructor([]) is { } defaultCtor) {
                this.body.Emit(OpCodes.Newobj, this.assemblyBuilder.Module.ImportReference(defaultCtor));    
            } else {
                var local = this.AllocLocal(typeRef);
                this.body.Emit(OpCodes.Ldloca, local);
                this.body.Emit(OpCodes.Initobj, typeRef);
                
                this.body.Emit(OpCodes.Ldloc, local);
            }
        }
    }

    private int AllocLocal(TypeReference typeRef) {
        var newVar = new VariableDefinition(typeRef);
        this.method.Body.Variables.Add(newVar);

        return newVar.Index;
    }

    private void BuildLocalAlloc(IR.LocalID at, IR.IType type) {
        var typeRef = this.assemblyBuilder.TypeBuilder.BuildType(type);
        
        var index = this.AllocLocal(typeRef);

        this.localMappings.Add(at, new LocalMapping(type, index));

        // for complex types, we need to zero-initialize them immediately because we might generate code
        // that creates references to their elements without initializing them, which the CLR will not like
        if (type.IsComplex()) {
            this.body.Emit(OpCodes.Ldloca, index);
            this.body.Emit(OpCodes.Initobj, typeRef);
        }
    }

    private void BuildCall(IR.IRef? outRef, IR.IValue target, IReadOnlyList<IR.IValue> argVals) {
        if (target is not IR.RefValue(var targetRef)) {
            throw new InvalidDataException("illegal instruction: call target value may only be a ref");
        }
        
        if (targetRef is IR.GlobalRef(IR.FunctionGlobalRef(var funcRef))) {
            if (this.library.Functions[funcRef.DefID].Signature().ResultType is not IR.NothingType) {
                this.StoreRef(outRef, () => this.EmitCall(funcRef, argVals));
            } else {
                this.EmitCall(funcRef, argVals);
            }    
        } else {
            if (this.GetRefType(targetRef) is not IR.FunctionType(var sig)) {
                throw new InvalidDataException($"illegal instruction: call target value {targetRef} is not a function value");
            }

            foreach (var argVal in argVals) {
                this.LoadValue(argVal);
            }
            
            // must be a value referencing a function pointer
            this.LoadValue(target);

            var returnTypeRef = this.assemblyBuilder.TypeBuilder.BuildType(sig.ResultType);
            var callSite = new CallSite(returnTypeRef) {
                HasThis = false,
                ExplicitThis = false,
                CallingConvention = MethodCallingConvention.Default,
            };

            foreach (var paramType in sig.ParameterTypes) {
                var paramTypeRef = this.assemblyBuilder.TypeBuilder.BuildType(paramType);
                callSite.Parameters.Add(new ParameterDefinition(paramTypeRef));
            }

            if (sig.ResultType is not IR.NothingType) {
                this.StoreRef(outRef, () => { this.body.Emit(OpCodes.Calli, callSite); });
            } else {
                this.body.Emit(OpCodes.Calli, callSite);
            }
        }
    }

    private void EmitCall(IR.FunctionRef funcRef, IEnumerable<IR.IValue> argVals) {
        foreach (var argVal in argVals) {
            this.LoadValue(argVal);
        }

        var funcMethod = this.assemblyBuilder.FunctionBuilder.FindFunctionMethod(funcRef) 
                         ?? throw new InvalidDataException($"invalid instruction: couldn't find function {funcRef.DefID}");

        this.body.Emit(OpCodes.Call, funcMethod);
    }

    private void BuildVirtualCall(
        IR.IRef? outRef,
        IR.IRef selfArg,
        IReadOnlyList<IR.IValue>? restArgs,
        IR.InterfaceRef ifaceRef,
        IR.MethodID methodID
    ) {
        var ifaceType = ifaceRef.ToObjectID().ToObjectType();
        this.assemblyBuilder.TypeBuilder.BuildType(ifaceType, out var ifaceTypeID);

        var ifaceDef = ((IR.DefInterfaceDecl)this.library.Metadata.Interfaces[ifaceRef.DefID]).Def;
        var ifaceMethod = ifaceDef.Methods[(int)methodID.ID];

        var methodRef = this.assemblyBuilder.TypeBuilder.GetInterfaceMethod(ifaceTypeID, methodID);

        if (ifaceMethod.ResultType is IR.NothingType) {
            this.EmitVirtualCall(methodRef, selfArg, restArgs);
        } else {
            this.StoreRef(outRef, () => this.EmitVirtualCall(methodRef, selfArg, restArgs));
        }
    }

    private void EmitVirtualCall(
        MethodReference methodRef,
        IR.IRef selfArg,
        IReadOnlyList<IR.IValue>? restArgs = null
    ) {
        this.LoadRef(selfArg);

        if (restArgs != null) {
            foreach (var argVal in restArgs) {
                this.LoadValue(argVal);
            }
        }

        this.body.Emit(OpCodes.Callvirt, methodRef);
    }

    private void LoadValue(IR.IValue loadValue) {
        switch (loadValue) {
            case IR.LiteralNilValue: {
                this.body.Emit(OpCodes.Ldnull);
                break;
            }
            case IR.LiteralValue<byte>(var val): {
                this.body.Emit(OpCodes.Ldc_I4, (int)val);
                break;
            }
            case IR.LiteralValue<sbyte>(var val): {
                this.body.Emit(OpCodes.Ldc_I4, (int)val);
                break;
            }
            case IR.LiteralValue<ushort>(var val): {
                this.body.Emit(OpCodes.Ldc_I4, (int)val);
                break;
            }
            case IR.LiteralValue<short>(var val): {
                this.body.Emit(OpCodes.Ldc_I4, (int)val);
                break;
            }
            case IR.LiteralValue<uint>(var val): {
                this.body.Emit(OpCodes.Ldc_I4, (int)val);
                break;
            }
            case IR.LiteralValue<int>(var val): {
                this.body.Emit(OpCodes.Ldc_I4, (int)val);
                break;
            }
            case IR.LiteralValue<ulong>(var val): {
                this.body.Emit(OpCodes.Ldc_I8, (long)val);
                break;
            }
            case IR.LiteralValue<long>(var val): {
                this.body.Emit(OpCodes.Ldc_I8, (long)val);
                break;
            }
            case IR.LiteralValue<bool>(var val): {
                this.body.Emit(OpCodes.Ldc_I4, val ? 1 : 0);
                break;
            }
            case IR.LiteralValue<nint>(var val): {
                this.body.Emit(OpCodes.Ldc_I8, val);
                break;
            }
            case IR.LiteralValue<nuint>(var val): {
                this.body.Emit(OpCodes.Ldc_I8, (long)val);
                break;
            }
            case IR.LiteralValue<float>(var val): {
                this.body.Emit(OpCodes.Ldc_R4, val);
                break;
            }
            case IR.LiteralValue<double>(var val): {
                this.body.Emit(OpCodes.Ldc_R8, val);
                break;
            }
            case IR.RefValue(var loadRef): {
                this.LoadRef(loadRef);
                break;
            }

            case IR.SizeOfValue(var type): {
                this.EmitSizeOf(type);
                break;
            }

            case IR.DefaultValue(var type): {
                this.EmitDefault(type);
                break;
            }

            default: {
                throw new NotSupportedException($"unsupported value: {loadValue}");
            }
        }
    }

    private void LoadRefAddr(IR.IRef ofRef) {
        switch (ofRef) {
            case IR.ResultRef: {
                this.body.Emit(OpCodes.Ldloca, this.GetResultIndex());
                
                break;
            }
            
            case IR.ArgRef(var arg): {
                var varIndex = this.GetArgIndex(arg);
                this.body.Emit(OpCodes.Ldarga, varIndex);

                break;
            }
            
            case IR.LocalRef(var local): {
                var varIndex = this.GetVariableIndex(local);
                this.body.Emit(OpCodes.Ldloca, varIndex);

                break;
            }

            case IR.Deref(var targetVal): {
                if (targetVal is not IR.RefValue(var targetRef)) {
                    throw new InvalidDataException($"invalid value for dereference instruction: {targetVal}");
                }

                // we can assume the value is a pointer or ref (to be dereferenced), so just load that
                this.LoadValue(targetVal);

                break;
            }

            case IR.GlobalRef(IR.StringLiteralGlobalRef): {
                throw new InvalidDataException("invalid instruction: can't address a string global");
            }
            
            case IR.GlobalRef(IR.FunctionGlobalRef): {
                throw new InvalidDataException("invalid instruction: can't address a function global");
            }
            
            case IR.GlobalRef(IR.VariableGlobalRef(var id)): {
                var fieldRef = this.assemblyBuilder.GetGlobalVariableRef(id);
                this.body.Emit(OpCodes.Ldsflda, fieldRef);
                break;
            }
            
            default: {
                throw new NotSupportedException($"unsupported ref: {ofRef}");
            }
        }
    }

    private void InsertJmp(IR.Label dest, OpCode code) {
        if (this.labelInstructions.TryGetValue(dest, out var instruction)) {
            this.body.Emit(code, instruction);
        } else {
            // create as a nop because we don't have an argument to be validated
            var unresolvedJmp = Instruction.Create(OpCodes.Nop);
            unresolvedJmp.OpCode = code;

            this.body.Append(unresolvedJmp);

            if (!this.unresolvedJmps.TryGetValue(dest, out var labelJmps)) {
                labelJmps = new List<Instruction>();
                this.unresolvedJmps.Add(dest, labelJmps);
            }
            
            labelJmps.Add(unresolvedJmp);
        }
    }

    private void InsertLabel(IR.Label label) {
        var instruction = Instruction.Create(OpCodes.Nop);
        this.body.Append(instruction);
        
        if (!this.labelInstructions.TryAdd(label, instruction)) {
            throw new InvalidDataException($"invalid instruction: duplicate label {label.ID}");
        }

        if (this.unresolvedJmps.Remove(label, out var jmps)) {
            foreach (var unresolvedJmp in jmps) {
                unresolvedJmp.Operand = instruction;
            }
        }
    }

    private IR.IType GetRefType(IR.IRef @ref) {
        var metadata = this.library.Metadata;

        switch (@ref) {
            case IR.ResultRef: {
                if (this.resultMapping.HasValue) {
                    return this.resultMapping.Value.Type;
                }

                break;
            }
            
            case IR.ArgRef(var id): {
                if (this.argMappings.TryGetValue(id, out var mapping)) {
                    return mapping.Type;
                }

                break;
            }
            
            case IR.LocalRef(var id): {
                if (this.localMappings.TryGetValue(id, out var mapping)) {
                    return mapping.Type;
                }

                break;
            }

            case IR.Deref(var target): {
                var targetType = this.GetValueType(target);
                var derefType = targetType.GetDerefType(); 
                if (derefType == null) {
                    break;
                }

                return derefType;
            }

            case IR.GlobalRef(IR.FunctionGlobalRef(var funcRef)): {
                if (!this.library.Functions.TryGetValue(funcRef.DefID, out var funcInfo)) {
                    throw new InvalidDataException($"reference to missing function {funcRef.DefID}");
                }

                return new IR.FunctionType(funcInfo.Signature());
            }
            
            case IR.GlobalRef(IR.VariableGlobalRef(var varID)): {
                if (metadata.Variables.TryGetValue(varID, out var varInfo)) {
                    return varInfo.Type;
                }
                break;
            }
            
            case IR.GlobalRef(IR.StringLiteralGlobalRef): {
                return IR.IType.String;
            }
            
            case IR.GlobalRef(IR.StaticFuncInfoGlobalRef): {
                return IR.IType.FunctionInfo;
            }
            
            case IR.GlobalRef(IR.StaticTypeInfoGlobalRef): {
                return IR.IType.TypeInfo;
            }
            
            case IR.GlobalRef(IR.StaticTagArrayGlobalRef): {
                return IR.IType.Any.MakeDynArray();
            }

            case IR.ElementRef(var elementRef): {
                var elementType = elementRef.InstanceType.GetElementType();
                if (elementType == null) {
                    throw new InvalidDataException($"invalid element instruction - {elementRef.InstanceType} is not an indexable type");
                }

                return elementType.MakeTempRef();
            }
            
            case IR.FieldRef(var fieldRef): {
                if (fieldRef.InstanceType is IR.ObjectType(IR.AnyClosureObjectID(var funcTypeID))
                    && fieldRef.FieldID == IR.FieldID.ClosurePointerField) {
                    var funcPtrType = (IR.IType)new IR.FunctionType(funcTypeID);
                    return funcPtrType.MakeTempRef();
                }

                var layoutField = this.assemblyBuilder.TypeBuilder.GetFieldRef(fieldRef.InstanceType, fieldRef.FieldID);
                return layoutField.Type.MakeTempRef();
            }

            case IR.VariantTagRef(var tagRef): {
                var field = this.assemblyBuilder.TypeBuilder.GetVariantTagFieldRef(tagRef.InstanceType);
                return field.Type.MakeTempRef();
            }
            
            case IR.VariantDataRef(var dataRef): {
                var caseLayout = this.assemblyBuilder.TypeBuilder.GetVariantDataFieldRef(
                    dataRef.InstanceType,
                    dataRef.CaseIndex
                );

                return caseLayout.Type.MakeTempRef();
            }
        }

        throw new InvalidDataException($"invalid instruction - type of {@ref} could not be determined this context");
    }

    public IR.IType GetValueType(IR.IValue val) {
        return val switch {
            IR.LiteralNilValue => IR.IType.Nothing.MakePointer(),
            IR.LiteralValue<bool> => IR.IType.Bool,
            IR.LiteralValue<sbyte> => IR.IType.I8,
            IR.LiteralValue<byte> => IR.IType.U8,
            IR.LiteralValue<short> => IR.IType.I16,
            IR.LiteralValue<ushort> => IR.IType.U16,
            IR.LiteralValue<int> => IR.IType.I32,
            IR.LiteralValue<uint> => IR.IType.U32,
            IR.LiteralValue<long> => IR.IType.I64,
            IR.LiteralValue<ulong> => IR.IType.U64,
            IR.LiteralValue<float> => IR.IType.F32,
            IR.LiteralValue<double> => IR.IType.F64,
            IR.LiteralValue<nint> => IR.IType.ISize,
            IR.LiteralValue<nuint> => IR.IType.USize,
            IR.RefValue(var @ref) => this.GetRefType(@ref),
            IR.SizeOfValue => IR.IType.I32,
            _ => throw new NotImplementedException($"value {val}"),
        };
    } 

    private void LoadRef(IR.IRef loadRef) {
        switch (loadRef) {
            case IR.ResultRef: {
                this.body.Emit(OpCodes.Ldloc, this.GetResultIndex());
                
                break;
            }
            
            case IR.ArgRef(var arg): {
                var varIndex = this.GetArgIndex(arg);
                this.body.Emit(OpCodes.Ldarg, varIndex);

                break;
            }
            
            case IR.LocalRef(var local): {
                var varIndex = this.GetVariableIndex(local);
                this.body.Emit(OpCodes.Ldloc, varIndex);

                break;
            }

            case IR.GlobalRef(IR.StringLiteralGlobalRef(var id)): {
                this.body.Emit(OpCodes.Ldsfld, this.assemblyBuilder.GetStringLiteralRef(id));
                break;
            }
            case IR.GlobalRef(IR.StaticTypeInfoGlobalRef(var closureID)): {
                this.LoadOptionalGlobal(this.assemblyBuilder.GetStaticTypeInfoFieldRef(closureID));
                break;
            }

            case IR.GlobalRef(IR.StaticFuncInfoGlobalRef(var id)): {
                this.LoadOptionalGlobal(this.assemblyBuilder.GetStaticFuncInfoFieldRef(id));
                break;
            }

            case IR.GlobalRef(IR.StaticTagArrayGlobalRef(var tagLoc)): {
                this.LoadOptionalGlobal(this.assemblyBuilder.GetStaticTagArrayFieldRef(tagLoc));
                break;
            }

            case IR.GlobalRef(IR.VariableGlobalRef(var id)): {
                this.body.Emit(OpCodes.Ldsfld, this.assemblyBuilder.GetGlobalVariableRef(id));
                break;
            }
            
            case IR.GlobalRef(IR.FunctionGlobalRef(var funcRef)): {
                var methodRef = this.assemblyBuilder.FunctionBuilder.FindFunctionMethod(funcRef)
                    ?? throw new InvalidDataException($"reference to missing function: {funcRef.ToString(this.library.Metadata)}");

                this.body.Emit(OpCodes.Ldftn, methodRef);
                break;
            }

            case IR.Deref(var atRef): {
                if (atRef is not IR.RefValue(var targetRef) 
                    || this.GetRefType(targetRef).GetDerefType() is not {} derefType) {
                    throw new InvalidDataException($"invalid value for dereference instruction: {atRef.ToString(this.library.Metadata)}");
                }

                var targetTypeRef = this.assemblyBuilder.TypeBuilder.BuildType(derefType);

                this.LoadValue(atRef);
                this.body.Emit(OpCodes.Ldobj, targetTypeRef);

                break;
            }

            case IR.DiscardRef: {
                throw new InvalidDataException("invalid instruction: can't load a discard");
            }

            case IR.FieldRef(var fieldRef): {
                this.LoadFieldRef(fieldRef.Instance, fieldRef.FieldID, fieldRef.InstanceType);
                return;
            }

            case IR.ElementRef(var elementRef): {
                this.LoadElementRef(elementRef.Instance, elementRef.Index, elementRef.InstanceType);
                return;
            }

            case IR.VariantTagRef(var tagRef): {
                var fieldRef = this.assemblyBuilder.TypeBuilder.GetVariantTagFieldRef(tagRef.InstanceType);
                this.LoadRefAddr(tagRef.Instance);
                this.body.Emit(OpCodes.Ldflda, fieldRef.Field);
                return;
            }
            
            case IR.VariantDataRef(var tagRef): {
                var fieldRef = this.assemblyBuilder.TypeBuilder.GetVariantDataFieldRef(tagRef.InstanceType, tagRef.CaseIndex);
                this.LoadRefAddr(tagRef.Instance);
                this.body.Emit(OpCodes.Ldflda, fieldRef.Field);
                return;
            }

            default: {
                throw new ArgumentOutOfRangeException(nameof(loadRef));
            }
        }
    }

    // RTTI globals are optional and may evaluate to null if RTTI is disabled 
    private void LoadOptionalGlobal(FieldReference? fieldRef) {
        if (fieldRef == null) {
            this.body.Emit(OpCodes.Ldnull);
        } else {
            this.body.Emit(OpCodes.Ldsfld, fieldRef);
        }
    }

    private void StoreRef(IR.IRef? storeRef, Action loadValue) {
        switch (storeRef) {
            case IR.ResultRef: {
                loadValue();
                
                var varIndex = this.GetResultIndex();
                this.body.Emit(OpCodes.Stloc, varIndex);

                break;
            }
            
            case IR.ArgRef(var id): {
                loadValue();
                
                var varIndex = this.GetArgIndex(id);
                this.body.Emit(OpCodes.Starg, varIndex);

                break;
            }
            
            case IR.LocalRef(var id): {
                loadValue();
                
                var varIndex = this.GetVariableIndex(id);
                this.body.Emit(OpCodes.Stloc, varIndex);

                break;
            }
            
            case IR.GlobalRef(IR.VariableGlobalRef(var varID)): {
                loadValue();

                var field = this.assemblyBuilder.GetGlobalVariableRef(varID);
                this.body.Emit(OpCodes.Stsfld, field);
                break;
            }

            case IR.GlobalRef(var globalRef): {
                throw new InvalidDataException($"invalid instruction: {globalRef} is not writeable");
            }

            case IR.Deref(var atRef): {
                if (atRef is not IR.RefValue(var targetRef)) {
                    throw new InvalidDataException($"invalid value for dereference instruction: {atRef}");
                }

                var refType = this.GetRefType(targetRef);
                if (refType.GetDerefType() is not { } derefType) {
                    throw new InvalidDataException($"invalid ref type for dereference instruction: {refType}");
                }
                
                var targetTypeRef = this.assemblyBuilder.TypeBuilder.BuildType(derefType);

                this.LoadValue(atRef);
                
                loadValue();

                this.body.Emit(OpCodes.Stobj, targetTypeRef);

                break;
            }

            case IR.DiscardRef or null: {
                loadValue();
                // nothing to do 
                this.body.Emit(OpCodes.Pop);
                break;
            }

            default: {
                throw new ArgumentOutOfRangeException(nameof(storeRef), "reference is not mutable");
            }
        }
    }
    
    private int GetResultIndex() {
        return this.resultMapping?.VariableIndex ?? 
            throw new InvalidDataException("invalid instruction: result variable was not found in this scope");
    }
    
    private int GetArgIndex(IR.ArgID arg) {
        if (this.argMappings.TryGetValue(arg, out var mapping)) {
            return mapping.VariableIndex;
        }

        throw new InvalidDataException($"invalid instruction: {arg} was not found in this scope");
    }

    private int GetVariableIndex(IR.LocalID local) {
        if (this.localMappings.TryGetValue(local, out var mapping)) {
            return mapping.VariableIndex;
        }

        throw new InvalidDataException($"invalid instruction: {local} was not found in this scope");
    }

    private void BuildBinOp(IR.BinOpInstruction binOp, OpCode op) {
        this.StoreRef(binOp.Out, () => {
            this.LoadValue(binOp.ArgA);
            this.LoadValue(binOp.ArgB);
            this.body.Emit(op);
        });
    }

    private void BuildCompareOrEqualOperation(IR.BinOpInstruction binOp, OpCode invCompareOp) {
        this.StoreRef(binOp.Out, () => {
            this.LoadValue(binOp.ArgA);
            this.LoadValue(binOp.ArgB);
            this.body.Emit(invCompareOp);
            
            this.body.Emit(OpCodes.Ldc_I4_0);
            this.body.Emit(OpCodes.Ceq);
        });
    }

    public void Finish() {
        if (this.unresolvedJmps.Count != 0) {
            var msg = new StringBuilder("illegal instructions: body contains unresolved jumps to labels: ");
            msg.AppendJoin(", ", this.unresolvedJmps.Keys);
            
            throw new InvalidDataException(msg.ToString());
        }
        
        if (this.HasReturnValue) {
            this.body.Emit(OpCodes.Ldloc, 0);
        }

        this.body.Emit(OpCodes.Ret);
    }
}
