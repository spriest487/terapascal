using System.Diagnostics;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace Terapascal.CIL;

public class FunctionBuilder {
    private readonly AssemblyBuilder assemblyBuilder;

    private readonly Dictionary<IR.FunctionRef, MethodReference> functionMethods;

    public FunctionBuilder(AssemblyBuilder assemblyBuilder) {
        this.assemblyBuilder = assemblyBuilder;

        this.functionMethods = new Dictionary<IR.FunctionRef, MethodReference>();
    }

    public void BuildFunctions(IR.Library lib) {
        var definedFuncs = new List<(MethodDefinition, IR.FunctionDef)>(lib.Functions.Count);

        var invocationParams = new List<IR.TypeParam>(8);

        foreach (var (id, func) in lib.Functions) {
            switch (func) {
                case IR.ExternalFunction(var externRef): {
                    this.ResolveExternalRef(id, externRef);
                    break;
                }

                case IR.LocalFunction(var def): {
                    if (!lib.Metadata.Functions.TryGetValue(id, out var funcInfo)) {
                        Debug.Print($"Missing function info: {id}");
                        continue;
                    }

                    // TODO: native generics
                    // only eagerly instantiate non-generic functions
                    invocationParams.Clear();
                    funcInfo.Identity.GetInvocationTypeParams(this.assemblyBuilder.LoadedMetadata, invocationParams);

                    if (invocationParams.Count == 0) {
                        var funcRef = id.ToFunctionRef([]);
                        var methodDef = this.CreateFunctionMethod(funcRef, def.Signature);
                        definedFuncs.Add((methodDef, def));
                    }
                    
                    break;
                }

                default: {
                    throw new ArgumentOutOfRangeException(nameof(func));
                }
            }
        }

        foreach (var (method, def) in definedFuncs) {
            try {
                this.BuildFunctionBody(lib, method, def);
            } catch (Exception e) {
                Debug.Print($"Failed to build function body of {method.Name}: {e}");
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
            Debug.Print($"Missing func info {implID} for method {implType.ToString(metadata)}.{method.Name}");
            return;
        }

        if (implFuncMetadata.Identity.TypeParams != null && implFuncMetadata.Identity.TypeParams.Count != 0) {
            // TODO: native generics
            return;
        }

        var implFuncRef = this.FindFunctionMethod(implID.ToFunctionRef([]))
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

    private void ResolveExternalRef(IR.FunctionID id, IR.ExternalFunctionRef externRef) {
        // external refs are never generic
        var functionRef = id.ToFunctionRef([]);

        // builtin methods
        if (externRef.Source == "rt") {
            var methodRef = this.assemblyBuilder.FindRuntimeFunction(externRef.Symbol);

            this.functionMethods.Add(functionRef, methodRef);
            return;
        }

        var externMethod = this.CreateFunctionMethod(functionRef, externRef.Signature);
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
    }

    private void BuildFunctionBody(IR.Library lib, MethodDefinition method, IR.FunctionDef def) {
        var builder = new InstructionBuilder(this.assemblyBuilder, lib, method);
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

    private MethodDefinition CreateFunctionMethod(IR.FunctionRef funcRef, IR.FunctionSig sig) {
        const MethodAttributes attrs = MethodAttributes.Static | MethodAttributes.Assembly;

        TypeDefinition typeDef;
        string name;

        if (this.assemblyBuilder.LoadedMetadata.FindFunction(funcRef.DefID, out var funcInfo)) {
            funcInfo = null;
        }

        switch (funcInfo?.Identity) {
            case IR.GlobalFunctionIdentity(var globalPath): {
                if (globalPath.GetParent() is { } unitPath) {
                    typeDef = this.assemblyBuilder.GetUnitClass(unitPath);
                } else {
                    typeDef = this.assemblyBuilder.GetInternalClass();
                }

                name = globalPath.Path.Last;
                break;
            }
                
            default: {
                name = FunctionMethodName(funcRef.DefID);
                typeDef = this.assemblyBuilder.GetInternalClass();
                break;
            }
        }           

        var methodDef = this.CreateMethodWithSig(name, attrs, sig);

        typeDef.Methods.Add(methodDef);

        this.functionMethods.Add(funcRef, methodDef);
        return methodDef;
    }

    public MethodReference? FindFunctionMethod(IR.FunctionRef funcRef) {
        return this.functionMethods.GetValueOrDefault(funcRef);
    }

    private static string FunctionMethodName(IR.FunctionID id) {
        return $"Function_{id.ID}";
    }
}
