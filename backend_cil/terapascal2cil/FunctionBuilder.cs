using Mono.Cecil;

namespace Terapascal.CIL;

public class FunctionBuilder {
    public const string FunctionsClassName = "Functions";
    
    private readonly IR.Library library;
    private readonly TerapascalAssemblyBuilder assemblyBuilder;
    private readonly TypeBuilder typeBuilder;

    private TypeDefinition? functionsClass;

    public FunctionBuilder(IR.Library library, TypeBuilder typeBuilder, TerapascalAssemblyBuilder assemblyBuilder) {
        this.library = library;
        this.typeBuilder = typeBuilder;
        this.assemblyBuilder = assemblyBuilder;
    }

    public TypeDefinition GetFreeFunctionClass() {
        if (this.functionsClass != null) {
            return this.functionsClass;
        }

        var objectType = this.assemblyBuilder.Module.TypeSystem.Object;
        this.functionsClass = new TypeDefinition(
            "", 
            FunctionsClassName,
            TypeAttributes.Sealed | TypeAttributes.Class, 
            objectType
        );

        this.assemblyBuilder.Module.Types.Add(this.functionsClass);

        return this.functionsClass;
    }

    public void TranslateFunction(IR.FunctionID id, IR.FunctionDef def) {
        var functionClass = this.GetFreeFunctionClass();

        var attrs = MethodAttributes.Static | MethodAttributes.Private;

        var method = new MethodDefinition(FunctionMethodName(id), attrs, this.assemblyBuilder.TypeSystem.Void);

        var builder = new InstructionBuilder(this.library, this.assemblyBuilder, method, this.typeBuilder, this);
        builder.BeginFunction(def);
        builder.AddInstructions(def.Body);
        builder.Return();

        functionClass.Methods.Add(method);
    }

    public static string FunctionMethodName(IR.FunctionID id) {
        return $"Function_{id.ID}";
    }
}
