using Mono.Cecil;
using Terapascal.Runtime;

namespace Terapascal.CIL;

public class FunctionBuilder {
    
    private readonly TerapascalAssemblyBuilder assemblyBuilder;
    private readonly TypeBuilder typeBuilder;

    private readonly Dictionary<IR.FunctionID, MethodReference> functionMethods;

    public FunctionBuilder(
        TypeBuilder typeBuilder,
        TerapascalAssemblyBuilder assemblyBuilder
    ) {
        this.typeBuilder = typeBuilder;
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
            this.BuildFunctionBody(lib, method, def);
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
        var builder = new InstructionBuilder(lib, this.assemblyBuilder, method, this.typeBuilder, this);
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
