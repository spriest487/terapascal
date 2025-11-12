using Mono.Cecil;
using Mono.Cecil.Cil;

namespace Terapascal.CIL;

public class FunctionBuilder {
    
    private readonly AssemblyBuilder assemblyBuilder;

    private readonly Dictionary<IR.FunctionID, MethodReference> functionMethods;

    public FunctionBuilder(AssemblyBuilder assemblyBuilder) {
        this.assemblyBuilder = assemblyBuilder;

        this.functionMethods = new Dictionary<IR.FunctionID, MethodReference>();
    }

    public void BuildFunctions(IR.Library lib) {
        var definedFuncs = new List<(MethodDefinition, IR.FunctionDef)>(lib.Functions.Count);

        foreach (var (id, func) in lib.Functions) {
            switch (func) {
                case IR.ExternalFunction(var externRef): {
                    this.ResolveExternalRef(id, externRef, lib);
                    break;
                }

                case IR.LocalFunction(var def): {
                    var methodDef = this.CreateFunctionMethod(id, def.Signature, lib);
                    definedFuncs.Add((methodDef, def));
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
                Console.Error.WriteLine($"Failed to build function body of {method.Name}: {e}");
            }
        }

        foreach (var (ifaceID, ifaceDecl) in lib.Metadata.Interfaces) {
            if (ifaceDecl is not IR.DefInterfaceDecl(var ifaceDef)) {
                continue;
            }

            var ifaceSelfType = ifaceID.InterfacePointerType();
            var ifaceTypeDef = this.assemblyBuilder.TypeBuilder.BuildTypeRef(ifaceSelfType, lib).Resolve();

            foreach (var (implType, impls) in ifaceDef.Implementations) {
                var implTypeDef = this.assemblyBuilder.TypeBuilder.BuildTypeRef(implType, lib).Resolve();

                foreach (var (methodID, implID) in impls.Methods) {
                    this.BuildInterfaceMethodImpl(ifaceDef, methodID, implID, implTypeDef, ifaceTypeDef, lib);
                }

                implTypeDef.Interfaces.Add(new InterfaceImplementation(ifaceTypeDef));
            }
        }
    }

    interface IA {
        int GetNumber();
    }

    class B : IA {
        public int GetNumber() => 123;
    }

    private void BuildInterfaceMethodImpl(
        IR.InterfaceDef ifaceDef,
        IR.MethodID methodID,
        IR.FunctionID implID,
        TypeDefinition implTypeDef,
        TypeDefinition ifaceTypeDef,
        IR.Library lib
    ) {
        var method = ifaceDef.Methods[(int)methodID.ID];
        var implFuncRef = this.FindFunctionMethod(implID)
            ?? throw new InvalidDataException($"missing function {implID.ID} in metadata");

        var implAttrs = MethodAttributes.Public 
            | MethodAttributes.HideBySig 
            | MethodAttributes.Virtual 
            | MethodAttributes.NewSlot 
            | MethodAttributes.Final;

        var implParams = new List<IR.IType>(method.Parameters.Count - 1);
        
        // impl methods' parameter lists implicitly include an initial self arg, which we skip here
        implParams.AddRange(method.Parameters.Skip(1));

        var methodSig = new IR.FunctionSig {
            ReturnType = method.ReturnType,
            ParameterTypes = implParams,
        };

        var implMethodDef = this.CreateMethodWithSig(method.Name, implAttrs, methodSig, lib);
        implMethodDef.HasThis = true;

        implTypeDef.Methods.Add(implMethodDef);

        implMethodDef.Overrides.Add(ifaceTypeDef.Methods[(int)methodID.ID]);

        implMethodDef.Body = new MethodBody(implMethodDef);
        var implBody = implMethodDef.Body.GetILProcessor();

        implBody.Emit(OpCodes.Ldarg_0);
        for (var i = 0; i < implMethodDef.Parameters.Count; i += 1) {
            implBody.Emit(OpCodes.Ldarg, i + 1);
        }

        // implBody.Emit(OpCodes.Tail);
        implBody.Emit(OpCodes.Call, implFuncRef);
        implBody.Emit(OpCodes.Ret);
    }

    private void ResolveExternalRef(IR.FunctionID id, IR.ExternalFunctionRef externRef, IR.Library library) {
        if (externRef.Source == "rt") {
            var methodRef = this.assemblyBuilder.FindRuntimeFunction(externRef.Symbol);

            this.functionMethods.Add(id, methodRef);
            return;
        }

        var externMethod = this.CreateFunctionMethod(id, externRef.Signature, library);
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
        builder.AddInstructions(def.Body);
        builder.Finish();
    }

    private MethodDefinition CreateMethodWithSig(
        string name,
        MethodAttributes attrs,
        IR.FunctionSig sig,
        IR.Library library
    ) {
        var returnTypeRef = this.assemblyBuilder.TypeBuilder.BuildTypeRef(sig.ReturnType, library);
        
        var methodDef = new MethodDefinition(name, attrs, returnTypeRef);

        foreach (var paramType in sig.ParameterTypes) {
            var paramTypeRef = this.assemblyBuilder.TypeBuilder.BuildTypeRef(paramType, library);

            methodDef.Parameters.Add(new ParameterDefinition(paramTypeRef));
        }

        return methodDef;
    }

    private MethodDefinition CreateFunctionMethod(IR.FunctionID id, IR.FunctionSig sig, IR.Library library) {
        const MethodAttributes attrs = MethodAttributes.Static | MethodAttributes.Assembly;

        var typeDef = this.assemblyBuilder.GetGlobalsClass();

        var methodDef = this.CreateMethodWithSig(FunctionMethodName(id), attrs, sig, library);

        typeDef.Methods.Add(methodDef);

        this.functionMethods.Add(id, methodDef);
        return methodDef;
    }

    public MethodReference? FindFunctionMethod(IR.FunctionID id) {
        return this.functionMethods.GetValueOrDefault(id);
    }

    private static string FunctionMethodName(IR.FunctionID id) {
        return $"Function_{id.ID}";
    }
}
