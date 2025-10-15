using Mono.Cecil;

namespace Terapascal.CIL;

public class FunctionBuilder {
    public const string FunctionsClassName = "Functions";
    
    private readonly IR.Library library;
    private readonly ModuleDefinition module;
    private readonly TypeBuilder typeBuilder;

    private TypeDefinition? functionsClass;

    public FunctionBuilder(IR.Library library, TypeBuilder typeBuilder, ModuleDefinition module) {
        this.library = library;
        this.typeBuilder = typeBuilder;
        this.module = module;
    }

    public TypeDefinition GetFreeFunctionClass() {
        if (this.functionsClass != null) {
            return this.functionsClass;
        }

        var objectType = this.module.TypeSystem.Object;
        this.functionsClass = new TypeDefinition(
            "", 
            FunctionsClassName,
            TypeAttributes.Sealed | TypeAttributes.Class, 
            objectType
        );

        this.module.Types.Add(this.functionsClass);

        return this.functionsClass;
    }

    public void TranslateFunction(IR.FunctionID id, IR.FunctionDef def) {
        var functionClass = this.GetFreeFunctionClass();

        var attrs = MethodAttributes.Static | MethodAttributes.Private;

        var method = new MethodDefinition(FunctionMethodName(id), attrs, this.module.TypeSystem.Void);

        var builder = new InstructionBuilder(this.library, this.module, method, this.typeBuilder, this);
        builder.BeginFunction(def);
        builder.AddInstructions(def.Body);
        builder.Return();

        functionClass.Methods.Add(method);
    }

    public static string FunctionMethodName(IR.FunctionID id) {
        return $"Function_{id.ID}";
    }
}
