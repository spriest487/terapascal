using System.Diagnostics;
using Mono.Cecil;

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
                    this.ResolveExternalRef(id, externRef);
                    break;
                }

                case IR.LocalFunction(var def): {
                    var methodDef = this.CreateFunctionMethod(id);
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

    private void ResolveExternalRef(IR.FunctionID id, IR.ExternalFunctionRef externRef) {
        if (externRef.Source != "rt") {
            throw new NotImplementedException("loading external libraries");
        }

        var methodRef = this.assemblyBuilder.FindRuntimeMethod(externRef.Symbol);

        this.functionMethods.Add(id, methodRef);
    }

    private void BuildFunctionBody(IR.Library lib, MethodDefinition method, IR.FunctionDef def) {
        var builder = new InstructionBuilder(this.assemblyBuilder, lib, method);
        builder.BeginFunction(def);
        builder.AddInstructions(def.Body);
        builder.Return();
    }

    private MethodDefinition CreateFunctionMethod(IR.FunctionID id) {
        var typeDef = this.assemblyBuilder.GetGlobalsClass();

        var attrs = MethodAttributes.Static | MethodAttributes.Private;

        var method = new MethodDefinition(FunctionMethodName(id), attrs, this.assemblyBuilder.TypeSystem.Void);

        typeDef.Methods.Add(method);

        this.functionMethods.Add(id, method);
        return method;
    }

    public MethodReference? FindFunctionMethod(IR.FunctionID id) {
        return this.functionMethods.GetValueOrDefault(id);
    }

    public static string FunctionMethodName(IR.FunctionID id) {
        return $"Function_{id.ID}";
    }
}
