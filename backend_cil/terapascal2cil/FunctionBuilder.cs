using System.Runtime.InteropServices;
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
        const MethodAttributes attrs = MethodAttributes.Static | MethodAttributes.Private;

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
