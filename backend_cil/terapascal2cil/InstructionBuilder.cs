using System.Text;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using static Terapascal.IR.TypeExt;
using static Terapascal.IR.FunctionExt;

namespace Terapascal.CIL;

using LocalMap = Dictionary<IR.LocalID, LocalMapping>;
using VarPoolCollection = Queue<int>;
using VarPool = Dictionary<IR.IType, Queue<int>>;

internal readonly record struct LocalMapping(IR.IType Type, int VariableIndex);

public class InstructionBuilder {
    private class LocalScope {
        public LocalMap LocalMappings { get; } = new LocalMap();
    }

    private readonly IR.Library library;

    private readonly ModuleDefinition module;

    private readonly MethodDefinition method;
    private readonly TypeBuilder typeBuilder;
    private readonly FunctionBuilder functionBuilder;

    private readonly Stack<LocalScope> scopes;

    private readonly ILProcessor body;

    private readonly VarPool varPool;

    private readonly Dictionary<IR.Label, Instruction> labelInstructions;
    private readonly Dictionary<IR.Label, List<Instruction>> unresolvedJmps;

    private bool hasReturn;

    public InstructionBuilder(
        IR.Library library,
        ModuleDefinition module,
        MethodDefinition method,
        TypeBuilder typeBuilder,
        FunctionBuilder functionBuilder
    ) {
        this.library = library;
        this.module = module;
        this.method = method;
        this.typeBuilder = typeBuilder;
        this.functionBuilder = functionBuilder;

        this.scopes = new Stack<LocalScope>();
        this.scopes.Push(new LocalScope());

        this.varPool = new VarPool();

        this.body = this.method.Body.GetILProcessor();

        this.labelInstructions = new Dictionary<IR.Label, Instruction>();
        this.unresolvedJmps = new Dictionary<IR.Label, List<Instruction>>();
    }

    public void BeginFunction(IR.FunctionDef function) {
        this.method.Body.InitLocals = true;
        
        if (this.scopes.Count != 1) {
            throw new InvalidOperationException("must be in the root scope");
        }
        
        var currentScope = this.scopes.Peek();

        var returnType = function.Signature.ReturnType;

        this.hasReturn = returnType is not IR.NothingType;

        // create a variable to hold the result variable %0 if there's a return type
        if (this.hasReturn) {
            this.method.ReturnType = this.typeBuilder.BuildTypeRef(returnType);
            
            var returnVar = new VariableDefinition(this.method.ReturnType);
            this.method.Body.Variables.Add(returnVar);

            var mapping = new LocalMapping(returnType, returnVar.Index);
            
            currentScope.LocalMappings.Add(new IR.LocalID(0), mapping);
        }

        var firstLocal = this.hasReturn ? 1UL : 0UL;
        
        var paramTypes = function.Signature.ParameterTypes;

        // params don't need local vars, we can reference them with ldarg
        for (var i = 0; i < paramTypes.Count; i += 1) {
            var paramLocal = new IR.LocalID(firstLocal + (ulong)i);
            var paramType = paramTypes[i];

            var paramTypeRef = this.typeBuilder.BuildTypeRef(paramType);
            this.method.Parameters.Add(new ParameterDefinition(paramTypeRef));

            // use negative "variable" indices to indicate arg positions
            var mapping = new LocalMapping(paramType, ~i);
            currentScope.LocalMappings.Add(paramLocal, mapping);
        }
    }

    public void AddInstructions(IReadOnlyList<IR.IInstruction> instructions) {
        foreach (var instruction in instructions) {
            switch (instruction) {
                case IR.CommentInstruction:
                case IR.DebugPushInstruction:
                case IR.DebugPopInstruction: {
                    // ignored
                    break;
                }

                case IR.RetainInstruction or IR.ReleaseInstruction: {
                    // RC: ignored?
                    break;
                }

                case IR.LocalBeginInstruction: {
                    this.scopes.Push(new LocalScope());
                    break;
                }

                case IR.LocalEndInstruction: {
                    var scope = this.scopes.Pop();

                    foreach (var (_, mapping) in scope.LocalMappings) {
                        if (!this.varPool.TryGetValue(mapping.Type, out var pool)) {
                            pool = new VarPoolCollection();
                            this.varPool.Add(mapping.Type, pool);
                        }

                        pool.Enqueue(mapping.VariableIndex);
                    }

                    break;
                }

                case IR.MoveInstruction { Out: var outRef, NewValue: var newVal }: {
                    this.LoadValue(newVal);
                    this.StoreRef(outRef);
                    break;
                }

                case IR.FieldInstruction {
                    Out: var outRef,
                    Arg: var argRef,
                    Field: var fieldID,
                    BaseType: var baseType,
                }: {
                    this.LoadRefAddr(argRef);

                    var fieldRef = this.typeBuilder.GetFieldRef(baseType, fieldID);

                    this.body.Emit(OpCodes.Ldflda, fieldRef);
                    this.body.Emit(OpCodes.Conv_U);
                    this.StoreRef(outRef);

                    break;
                }

                case IR.ElementInstruction {
                    Out: var outRef,
                    Arg: var arrRef,
                    Index: var indexVal,
                    ElementType: var elementType,
                }: {
                    // TODO: fix element instruction to provide array struct type instead?
                    var elementTypeRef = this.typeBuilder.BuildTypeRef(elementType)
                        .Resolve();
                    var elementSize = elementTypeRef?.ClassSize ?? TypeBuilder.PointerSize;

                    this.LoadRefAddr(arrRef);
                    this.body.Emit(OpCodes.Conv_U);

                    this.body.Emit(OpCodes.Ldc_I4, elementSize);
                    this.LoadValue(indexVal);
                    this.body.Emit(OpCodes.Mul);

                    this.body.Emit(OpCodes.Add);

                    this.StoreRef(outRef);

                    break;
                }

                case IR.VariantTagInstruction {
                    Out: var outRef,
                    Arg: var argRef,
                    VariantType: var variantType,
                }: {
                    this.LoadRef(argRef);

                    var fieldRef = this.typeBuilder.GetVariantDiscriminatorFieldRef(variantType);

                    this.body.Emit(OpCodes.Ldfld, fieldRef);
                    this.StoreRef(outRef);

                    break;
                }

                case IR.VariantDataInstruction {
                    Out: var outRef,
                    Arg: var argRef,
                    VariantType: var variantType,
                    Tag: var tag,
                }: {
                    this.LoadRefAddr(argRef);

                    var fieldRef = this.typeBuilder.GetVariantDataFieldRef(variantType, tag);

                    this.body.Emit(OpCodes.Ldflda, fieldRef);
                    this.body.Emit(OpCodes.Conv_U);
                    this.StoreRef(outRef);

                    break;
                }

                case IR.RcNewInstruction {
                    Out: var outRef,
                    TypeID: var typeID,
                }: {
                    this.BuildRcNew(outRef, typeID);

                    break;
                }

                case IR.CastInstruction {
                    Out: var outRef,
                    Value: var val,
                    Type: var castToType,
                }: {
                    this.LoadValue(val);

                    if (castToType.IsClass()) {
                        var typeRef = this.typeBuilder.BuildTypeRef(castToType);
                        this.body.Emit(OpCodes.Castclass, typeRef);
                    }

                    this.StoreRef(outRef);
                    break;
                }

                case IR.ClassIsInstruction {
                    Out: var outRef,
                    Arg: var argVal,
                    ClassID: var classID,
                }: {
                    var classTypeRef = this.typeBuilder.BuildTypeRef(new IR.RcPointerType(classID));

                    this.LoadValue(argVal);
                    this.body.Emit(OpCodes.Isinst, classTypeRef);
                    this.StoreRef(outRef);

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
                    this.LoadRefAddr(argRef);
                    this.StoreRef(outRef);
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
                    this.BuildBinOp(op, OpCodes.Cgt);
                    this.BuildBinOp(op, OpCodes.Ceq);
                    this.body.Emit(OpCodes.Or);
                    break;
                }

                case IR.LtInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Clt);
                    break;
                }

                case IR.LteInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Clt);
                    this.BuildBinOp(op, OpCodes.Ceq);
                    this.body.Emit(OpCodes.Or);
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
                    this.LoadValue(op.Arg);
                    this.body.Emit(OpCodes.Ldc_I4_0);
                    this.body.Emit(OpCodes.Ceq);
                    this.StoreRef(op.Out);
                    break;
                }

                case IR.BitNotInstruction(var op): {
                    this.LoadValue(op.Arg);
                    this.body.Emit(OpCodes.Not);
                    break;
                }

                case IR.AddInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Add);
                    break;
                }

                case IR.SubInstruction(var op): {
                    this.BuildBinOp(op, OpCodes.Sub);
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

    private void BuildRcNew(IR.IRef outRef, IR.TypeDefID typeID) {
        var classID = new IR.ClassVirtualTypeID(typeID);
        var classTypeRef = this.typeBuilder.BuildTypeRef(new IR.RcPointerType(classID));
        var classTypeDef = classTypeRef.Resolve();

        var defaultCtor = classTypeDef.GetConstructors()
                              .SingleOrDefault(ctor => ctor.Parameters.Count == 0)
                          ?? throw new InvalidOperationException(
                              $"invalid instruction: type {classTypeRef} cannot be constructed");

        this.body.Emit(OpCodes.Newobj, defaultCtor);
        this.StoreRef(outRef);
    }

    private void BuildRaise(IR.IRef val) {
        var exceptionType = this.typeBuilder.ResolveCore(this.typeBuilder.ExceptionType);
        var ctor = (MethodReference)exceptionType
            .GetConstructors()
            .Single(ctor
                => ctor.Parameters.Count == 1
                   && ctor.Parameters[0].ParameterType.FullName == this.typeBuilder.StringType.FullName);
        ctor = this.typeBuilder.ImportCoreReference(ctor);

        this.LoadRef(val);
        this.body.Emit(OpCodes.Newobj, ctor);
        this.body.Emit(OpCodes.Throw);
    }

    private void BuildLocalAlloc(IR.LocalID at, IR.IType type) {
        if (!this.varPool.TryGetValue(type, out var pool) || !pool.TryDequeue(out var index)) {
            var typeRef = this.typeBuilder.BuildTypeRef(type);

            var newVar = new VariableDefinition(typeRef);
            this.method.Body.Variables.Add(newVar);

            index = newVar.Index;
        }

        var scope = this.scopes.Peek();
        scope.LocalMappings.Add(at, new LocalMapping(type, index));
    }

    private void BuildCall(IR.IRef? outRef, IR.IValue funcVal, IReadOnlyList<IR.IValue> argVals) {
        foreach (var argVal in argVals) {
            this.LoadValue(argVal);
        }

        var funcID = funcVal switch {
            IR.RefValue(IR.GlobalRef(IR.FunctionGlobalRef(var id))) => id,
            _ => throw new NotImplementedException("call to non-function value"),
        };

        var sig = this.library.Functions[funcID].Signature();
        var returnTypeRef = this.typeBuilder.BuildTypeRef(sig.ReturnType);

        var funcClass = this.functionBuilder.GetFreeFunctionClass();
        var methodName = FunctionBuilder.FunctionMethodName(funcID);
        var funcRef = new MethodReference(methodName, returnTypeRef, funcClass) {
            HasThis = false,
        };

        this.body.Emit(OpCodes.Call, funcRef);
        if (outRef != null) {
            this.StoreRef(outRef);
        } else {
            this.body.Emit(OpCodes.Pop);
        }
    }

    private void LoadValue(IR.IValue loadValue) {
        switch (loadValue) {
            case IR.LiteralNullValue: {
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
                this.body.Emit(OpCodes.Ldind_I, val);
                break;
            }
            case IR.LiteralValue<nuint>(var val): {
                this.body.Emit(OpCodes.Ldind_I, (long)val);
                break;
            }

            case IR.RefValue(var loadRef): {
                this.LoadRef(loadRef);
                break;
            }

            case IR.SizeOfValue(var type): {
                var typeRef = this.typeBuilder.BuildTypeRef(type);

                var typeDef = typeRef.Resolve();
                if (typeDef == null) {
                    throw new InvalidOperationException($"invalid SizeOf instruction: references unresolvable type {type}");
                }

                var size = typeDef.ClassSize;
                if (size < 0) {
                    size = 0;
                    // TODO: should be enforced by the frontend if needed? might need array instructions
                    // throw new InvalidOperationException($"invalid SizeOf instruction: type {type} does not have a fixed size");
                }
                
                this.body.Emit(OpCodes.Ldc_I4, size);
                break;
            }

            default: {
                throw new ArgumentOutOfRangeException(nameof(loadValue));
            }
        }
    }

    private void LoadRefAddr(IR.IRef ofRef) {
        switch (ofRef) {
            case IR.LocalRef(var local): {
                var varIndex = this.GetVariableIndex(local);
                if (varIndex >= 0) {
                    this.body.Emit(OpCodes.Ldloca, varIndex);
                } else {
                    this.body.Emit(OpCodes.Ldarga, ~varIndex);
                }

                break;
            }

            case IR.Deref(var targetVal): {
                // we can assume the value is a pointer (to be dereferenced), so just load that
                this.LoadValue(targetVal);
                break;
            }

            case IR.GlobalRef(IR.StringLiteralGlobalRef): {
                throw new InvalidDataException("invalid instruction: can't address a string global");
            }
            
            case IR.GlobalRef(IR.FunctionGlobalRef): {
                throw new InvalidDataException("invalid instruction: can't address a function global");
            }
            
            case IR.GlobalRef(IR.StaticClosureGlobalRef): {
                throw new InvalidDataException("invalid instruction: can't address a closure global");
            }
            
            case IR.GlobalRef(IR.VariableGlobalRef): {
                // not implemented
                this.body.Emit(OpCodes.Ldnull);
                break;
            }
            
            default: {
                throw new ArgumentOutOfRangeException(nameof(ofRef));
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
        switch (@ref) {
            case IR.LocalRef(var id): {
                foreach (var scope in this.scopes) {
                    if (scope.LocalMappings.TryGetValue(id, out var mapping)) {
                        return mapping.Type;
                    }
                }

                break;
            }

            case IR.GlobalRef(IR.FunctionGlobalRef(var funcID)): {
                if (this.library.Functions.TryGetValue(funcID, out var func)) {
                    var typeID = this.library.Metadata.FindFunctionType(func.Signature());
                    if (typeID != null) {
                        return new IR.FunctionType(typeID.Value);
                    } 
                }

                break;
            }
            
            case IR.GlobalRef(IR.VariableGlobalRef(var varID)): {
                if (this.library.Variables.TryGetValue(varID, out var varType)) {
                    return varType;
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
                if (this.library.Metadata.DynArrayStructs.TryGetValue(IR.IType.Any, out var classID)) {
                    return new IR.RcPointerType(new IR.ClassVirtualTypeID(classID));
                }

                break;
            }

            case IR.GlobalRef(IR.StaticClosureGlobalRef(var id)): {
                var closureInfo = this.library.StaticClosures.FirstOrDefault(closure => closure.ID == id);
                if (closureInfo != null) {
                    return new IR.FunctionType(closureInfo.FunctionTypeID);
                }

                break;
            }
        }

        throw new InvalidDataException($"invalid instruction - type of {@ref} could not be determined this context");
    }

    private void LoadRef(IR.IRef loadRef) {
        switch (loadRef) {
            case IR.LocalRef localRef: {
                var varIndex = this.GetVariableIndex(localRef.ID);
                if (varIndex >= 0) {
                    this.body.Emit(OpCodes.Ldloc, varIndex);
                } else {
                    this.body.Emit(OpCodes.Ldarg, ~varIndex);
                }

                break;
            }

            case IR.GlobalRef(var globalRef): {
                // not implemented
                this.body.Emit(OpCodes.Ldnull);
                break;
            }

            case IR.Deref(var atRef): {
                if (atRef is not IR.RefValue(var targetRef) 
                    || this.GetRefType(targetRef) is not IR.PointerType(var inner)) {
                    throw new InvalidDataException($"invalid value for dereference instruction: {atRef}");
                }

                var targetTypeRef = this.typeBuilder.BuildTypeRef(inner);
                
                this.LoadValue(atRef);
                this.body.Emit(OpCodes.Ldobj, targetTypeRef);
                
                break;
            }

            case IR.DiscardRef: {
                throw new InvalidDataException("invalid instruction: can't load a discard");
            }

            default: {
                throw new ArgumentOutOfRangeException(nameof(loadRef));
            }
        }
    }

    private void StoreRef(IR.IRef storeRef) {
        switch (storeRef) {
            case IR.LocalRef localRef: {
                var varIndex = this.GetVariableIndex(localRef.ID);
                if (varIndex >= 0) {
                    this.body.Emit(OpCodes.Stloc, varIndex);
                } else {
                    this.body.Emit(OpCodes.Starg, -varIndex);
                }

                break;
            }

            case IR.GlobalRef(var globalRef): {
                // not implemented
                this.body.Emit(OpCodes.Pop);
                break;
            }

            case IR.Deref(var atRef): {
                if (atRef is not IR.RefValue(var targetRef) 
                    || this.GetRefType(targetRef) is not IR.PointerType(var inner)) {
                    throw new InvalidDataException($"invalid value for dereference instruction: {atRef}");
                }

                var targetTypeRef = this.typeBuilder.BuildTypeRef(inner);
                
                this.LoadValue(atRef);
                this.body.Emit(OpCodes.Stobj, targetTypeRef);
                break;
            }

            case IR.DiscardRef: {
                // nothing to do 
                this.body.Emit(OpCodes.Pop);
                break;
            }

            default: {
                throw new ArgumentOutOfRangeException(nameof(storeRef));
            }
        }
    }

    private int GetVariableIndex(IR.LocalID local) {
        foreach (var scope in this.scopes) {
            if (scope.LocalMappings.TryGetValue(local, out var mapping)) {
                return mapping.VariableIndex;
            }
        }

        throw new InvalidDataException($"invalid instruction: local {local.ID} was not found in this scope");
    }

    private void BuildBinOp(IR.BinOpInstruction binOp, OpCode op) {
        this.LoadValue(binOp.ArgA);
        this.LoadValue(binOp.ArgB);
        this.body.Emit(op);
        this.StoreRef(binOp.Out);
    }

    public void Return() {
        if (this.unresolvedJmps.Count != 0) {
            var msg = new StringBuilder("illegal instructions: body contains unresolved jumps to labels: ");
            msg.AppendJoin(", ", this.unresolvedJmps.Keys);
            
            throw new InvalidDataException(msg.ToString());
        }
        
        if (this.hasReturn) {
            this.body.Emit(OpCodes.Ldloc, 0);
        }

        this.body.Emit(OpCodes.Ret);
    }
}
