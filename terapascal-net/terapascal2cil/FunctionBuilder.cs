using Mono.Cecil;
using Mono.Cecil.Cil;

namespace Terapascal.CIL;

public class FunctionBuilder {
    private readonly AssemblyBuilder assemblyBuilder;

    private readonly SortedDictionary<IR.FunctionID, IR.FunctionDef> functionDefs;

    private readonly Queue<IR.FunctionRef> funcsToBuild;
    private readonly Dictionary<IR.FunctionRef, MethodReference> funcInstances;

    public FunctionBuilder(AssemblyBuilder assemblyBuilder) {
        this.assemblyBuilder = assemblyBuilder;

        this.functionDefs = new SortedDictionary<IR.FunctionID, IR.FunctionDef>();

        this.funcInstances = new Dictionary<IR.FunctionRef, MethodReference>();
        this.funcsToBuild = new Queue<IR.FunctionRef>();
    }

    public void BuildFunctions(IR.Library lib) {
        var invocationParams = new List<IR.TypeParam>(8);

        foreach (var (id, func) in lib.Functions) {
            switch (func) {
                case IR.ExternalFunction(var externRef): {
                    var methodRef = this.ResolveExternalRef(id, externRef);
                    this.funcInstances.Add(id.ToFunctionRef([]), methodRef);
                    break;
                }

                case IR.LocalFunction(var def): {
                    this.functionDefs.Add(id, def);

                    if (!lib.Metadata.Functions.TryGetValue(id, out var funcInfo)) {
                        Console.Error.WriteLine($"Missing function info: {id}");
                        continue;
                    }

                    // TODO: native generics
                    // eagerly instantiate non-generic functions
                    invocationParams.Clear();
                    funcInfo.Identity.GetInvocationTypeParams(this.assemblyBuilder.LoadedMetadata, invocationParams);

                    if (invocationParams.Count == 0) {
                        var funcRef = id.ToFunctionRef([]);
                        var methodDef = this.CreateFunctionMethod(funcRef);

                        this.funcInstances.Add(funcRef, methodDef);
                        this.funcsToBuild.Enqueue(funcRef);
                    }
                    
                    break;
                }

                default: {
                    throw new ArgumentOutOfRangeException(nameof(func));
                }
            }
        }

        foreach (var (selfType, impls) in lib.Metadata.InterfaceImpls) {
            // TODO: native generics
            if (selfType.ContainsGenericParams) {
                continue;
            }

            foreach (var (ifaceRef, ifaceImpl) in impls) {
                if (!lib.Metadata.Interfaces.TryGetValue(ifaceRef.DefID, out var ifaceDecl)
                    || ifaceDecl is not IR.DefInterfaceDecl(var ifaceDef)
                ) {
                    throw new InvalidDataException($"missing interface def for implemented interface {ifaceRef.DefID}");
                }

                var ifaceType = ifaceRef.ToObjectID().ToObjectType();
                var ifaceTypeDef = this.assemblyBuilder.TypeBuilder.BuildType(ifaceType).Resolve();
                
                foreach (var (methodID, implID) in ifaceImpl.Methods) {
                    this.BuildInterfaceMethodImpl(ifaceDef, ifaceTypeDef, methodID, implID, selfType);
                }
            }
        }
    }

    private void BuildInterfaceMethodImpl(
        IR.InterfaceDef ifaceDef,
        TypeDefinition ifaceTypeDef,
        IR.MethodID methodID,
        IR.FunctionID implID,
        IR.IType implType
    ) {
        var metadata = this.assemblyBuilder.LoadedMetadata;

        var implTypeDef = this.assemblyBuilder.TypeBuilder.BuildType(implType).Resolve();

        var method = ifaceDef.Methods[(int)methodID.ID];
        if (!metadata.FindFunction(implID, out var implFuncMetadata)) {
            Console.Error.WriteLine($"Missing func info {implID} for method {implType.ToString(metadata)}.{method.Name}");
            return;
        }

        if (implFuncMetadata.Identity.TypeParams != null && implFuncMetadata.Identity.TypeParams.Count != 0) {
            // TODO: native generics
            return;
        }

        var implFuncRef = this.GetFunctionMethod(implID.ToFunctionRef([]))
            ?? throw new InvalidDataException($"missing function {implID.ID} in metadata");

        var implAttrs = MethodAttributes.Public 
            | MethodAttributes.HideBySig 
            | MethodAttributes.Virtual 
            | MethodAttributes.NewSlot 
            | MethodAttributes.Final;

        var implParams = new List<IR.IType>(method.Params.Count - 1);

        // impl methods' parameter lists implicitly include an initial self arg, which we skip here
        implParams.AddRange(method.Params.Skip(1).Select(p => p.Type));

        var methodSig = new IR.FunctionSig {
            ResultType = method.ResultType,
            ParameterTypes = implParams,
        };

        var implMethodDef = this.CreateMethodWithSig(method.Name, implAttrs, methodSig);
        implMethodDef.HasThis = true;

        implTypeDef.Methods.Add(implMethodDef);

        implMethodDef.Overrides.Add(ifaceTypeDef.Methods[(int)methodID.ID]);

        implMethodDef.Body = new MethodBody(implMethodDef);
        var implBody = implMethodDef.Body.GetILProcessor();

        // if self is a value type, the implementing function's self arg will be implicitly by-ref
        if (implType.IsObjectType()) {
            implBody.Emit(OpCodes.Ldarg_0);
        } else {
            implBody.Emit(OpCodes.Ldarga, 0);
        }
        
        for (var i = 0; i < implMethodDef.Parameters.Count; i += 1) {
            implBody.Emit(OpCodes.Ldarg, i + 1);
        }

        // implBody.Emit(OpCodes.Tail);
        implBody.Emit(OpCodes.Call, implFuncRef);
        implBody.Emit(OpCodes.Ret);
    }

    private MethodReference ResolveExternalRef(IR.FunctionID id, IR.ExternalFunctionRef externRef) {
        // external refs are never generic
        var functionRef = id.ToFunctionRef([]);

        // builtin methods
        if (externRef.Source == "rt") {
            return this.assemblyBuilder.FindRuntimeFunction(externRef.Symbol);
        }

        var externMethod = this.CreateFunctionMethod(functionRef);
        externMethod.Attributes |= MethodAttributes.PInvokeImpl;
        externMethod.ImplAttributes |= MethodImplAttributes.PreserveSig;

        const PInvokeAttributes pInvokeAttrs = PInvokeAttributes.CallConvCdecl 
            | PInvokeAttributes.BestFitDisabled 
            | PInvokeAttributes.CharSetAnsi 
            | PInvokeAttributes.NoMangle;

        var moduleRefs = this.assemblyBuilder.Module.ModuleReferences;

        var moduleRef = moduleRefs.FirstOrDefault(x => x.Name == externRef.Source); 
        if (moduleRef == null) {
            moduleRef = new ModuleReference(externRef.Source);
            this.assemblyBuilder.Module.ModuleReferences.Add(moduleRef);
        }

        externMethod.PInvokeInfo = new PInvokeInfo(pInvokeAttrs, externRef.Symbol, moduleRef);

        externMethod.Body = null;

        return externMethod;
    }

    public void BuildFunctionBodies() {
        while (this.funcsToBuild.TryDequeue(out var funcRef)) {
            if (!this.funcInstances.TryGetValue(funcRef, out var methodRef)
                || methodRef is not MethodDefinition methodDef
            ) {
                var funcDisplay = funcRef.ToString(this.assemblyBuilder.LoadedMetadata);
                throw new InvalidOperationException($"func ref {funcDisplay} was queued for building but has no definition");
            }

            try {
                this.BuildFunctionBody(funcRef, methodDef);
            } catch (Exception e) {
                Console.Error.WriteLine($"Failed to build function body of {methodDef.Name}: {e}");

                methodDef.DeclaringType.Methods.Remove(methodDef);
            }
        }
    }

    private void BuildFunctionBody(IR.FunctionRef funcRef, MethodDefinition methodDef) {
        var metadata = this.assemblyBuilder.LoadedMetadata;

        // if there's no IR definition for this function, this is an error, because
        if (!this.functionDefs.TryGetValue(funcRef.DefID, out var def)) {
            var msg = $"reference to {funcRef.ToString(metadata)} has no corresponding function definition";
            throw new InvalidDataException(msg);
        }

        if (!metadata.FindFunction(funcRef.DefID, out var funcInfo)) {
            var msg = $"reference to {funcRef.ToString(metadata)} has no corresponding function metadata";
            throw new InvalidDataException(msg);
        }

        // TODO: native generics
        // monomorphize any generic defs
        if (funcRef.HasTypeArgs) {
            var typeParams = funcInfo.Identity.TypeParams ?? [];
            if (typeParams.Count != funcRef.TypeArgs.Count) {
                throw new InvalidDataException($"reference to {funcRef.ToString(metadata)} with incorrect type arg count");
            }

            var typeMap = IR.Util.BuildGenericTypeMap(typeParams, funcRef.TypeArgs);

            def = def.ResolveGeneric(typeMap);
        }

        var builder = new InstructionBuilder(this.assemblyBuilder, methodDef);
        builder.BeginFunction(def);
        builder.AddInstructions(def.Body.Instructions);
        builder.Finish();
    }

    private MethodDefinition CreateMethodWithSig(
        string name,
        MethodAttributes attrs,
        IR.FunctionSig sig
    ) {
        var returnTypeRef = this.assemblyBuilder.TypeBuilder.BuildType(sig.ResultType);

        var methodDef = new MethodDefinition(name, attrs, returnTypeRef);

        foreach (var paramType in sig.ParameterTypes) {
            var paramTypeRef = this.assemblyBuilder.TypeBuilder.BuildType(paramType);

            methodDef.Parameters.Add(new ParameterDefinition(paramTypeRef));
        }

        return methodDef;
    }

    private MethodDefinition CreateFunctionMethod(IR.FunctionRef funcRef) {
        const MethodAttributes attrs = MethodAttributes.Static | MethodAttributes.Assembly;

        TypeDefinition declaringTypeDef;
        string name;

        if (!this.assemblyBuilder.LoadedMetadata.FindFunction(funcRef.DefID, out var funcInfo)) {
            var msg = $"missing function metadata for referenced function {funcRef.ToString(this.assemblyBuilder.LoadedMetadata)}";
            throw new InvalidDataException(msg);
        }

        var sig = new IR.FunctionSig {
            ResultType = funcInfo.ResultType,
            ParameterTypes = funcInfo.Params.Select(p => p.Type).ToArray(),
        };

        if (funcRef.TypeArgs is { Count: > 0 }) {
            var typeParams = funcInfo.Identity.TypeParams;
            if (typeParams == null || typeParams.Count != funcRef.TypeArgs.Count) {
                var msg = $"expected {typeParams?.Count ?? 0} type params for function referenced by {funcRef.ToString(this.assemblyBuilder.LoadedMetadata)}";
                throw new InvalidDataException(msg);
            }

            var typeMap = IR.Util.BuildGenericTypeMap(typeParams, funcRef.TypeArgs);
            sig = sig.ResolveGeneric(typeMap);
        }

        switch (funcInfo.Identity) {
            case IR.GlobalFunctionIdentity(var globalPath): {
                if (globalPath.GetParent() is { } unitPath) {
                    declaringTypeDef = this.assemblyBuilder.GetUnitClass(unitPath);
                } else {
                    declaringTypeDef = this.assemblyBuilder.GetInternalClass();
                }

                name = globalPath.Path.Last;
                break;
            }

            case IR.InternalFunctionIdentity internalIdentity: {
                name = internalIdentity.Name;
                declaringTypeDef = this.assemblyBuilder.GetInternalClass();
                break;
            }
                
            default: {
                name = FunctionMethodName(funcRef.DefID);
                declaringTypeDef = this.assemblyBuilder.GetInternalClass();
                break;
            }
        }

        var methodDef = this.CreateMethodWithSig(name, attrs, sig);
        declaringTypeDef.Methods.Add(methodDef);

        return methodDef;
    }

    /// <summary>
    /// Get or create the CLR method which the given function reference is translated to.
    /// If the function hasn't been translated yet, an empty method will be added to be populated later.
    /// </summary>
    public MethodReference GetFunctionMethod(IR.FunctionRef funcRef) {
        if (this.funcInstances.TryGetValue(funcRef, out var methodRef)) {
            return methodRef;
        }

        methodRef = this.CreateFunctionMethod(funcRef);
        this.funcsToBuild.Enqueue(funcRef);

        return methodRef;
    }

    private static string FunctionMethodName(IR.FunctionID id) {
        return $"Function_{id.ID}";
    }
}
