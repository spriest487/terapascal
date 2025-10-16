using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

namespace Terapascal.CIL;

public class AssemblyBuilder {
    public const string GlobalsClassName = "Globals";

    public AssemblyDefinition CoreLibrary { get; }
    public AssemblyDefinition StandardLibrary { get; }
    public AssemblyDefinition RuntimeLibrary { get; }
    public AssemblyDefinition Assembly { get; }
    
    public TypeBuilder TypeBuilder { get; }
    public FunctionBuilder FunctionBuilder { get; }

    public ModuleDefinition Module => this.Assembly.MainModule;
    public TypeSystem TypeSystem => this.Assembly.MainModule.TypeSystem;

    private TypeDefinition? globalsClass;
    private TypeDefinition? systemFuncsClass;

    private readonly Dictionary<IR.StringID, FieldDefinition> stringLitFields =
        new Dictionary<IR.StringID, FieldDefinition>();

    private readonly Dictionary<IR.VariableID, FieldDefinition> globalVarFields =
        new Dictionary<IR.VariableID, FieldDefinition>();

    public AssemblyBuilder(string assemblyName, Version assemblyVersion, string refLibPath) {
        this.Assembly = AssemblyDefinition.CreateAssembly(
            new AssemblyNameDefinition(assemblyName, assemblyVersion),
            assemblyName,
            new ModuleParameters {
                Kind = ModuleKind.Dll,
                Runtime = TargetRuntime.Net_2_0,
            });

        this.CoreLibrary = AssemblyDefinition.ReadAssembly(Path.Join(refLibPath, "mscorlib.dll"));
        this.StandardLibrary = AssemblyDefinition.ReadAssembly(Path.Join(refLibPath, "netstandard.dll"));

        this.Assembly.MainModule.AssemblyReferences.Add(this.CoreLibrary.Name);
        this.Assembly.MainModule.AssemblyReferences.Add(this.StandardLibrary.Name);

        this.RuntimeLibrary = AssemblyDefinition.ReadAssembly("Terapascal.Runtime.dll");

        this.TypeBuilder = new TypeBuilder(this);
        this.FunctionBuilder = new FunctionBuilder(this);
    }

    public TypeDefinition GetGlobalsClass() {
        if (this.globalsClass != null) {
            return this.globalsClass;
        }

        var ns = this.Assembly.Name.Name;

        var objectType = this.Module.TypeSystem.Object;
        this.globalsClass = new TypeDefinition(
            ns,
            GlobalsClassName,
            TypeAttributes.Sealed | TypeAttributes.Class,
            objectType
        );
        
        this.Module.Types.Add(this.globalsClass);

        return this.globalsClass;
    }

    private MethodDefinition GetGlobalsCCtor() {
        var globalsClass = this.GetGlobalsClass();
        var cctor = globalsClass.GetStaticConstructor();
        if (cctor != null) {
            return cctor;
        }

        var methodDef = new MethodDefinition(
            ".cctor",
            MethodAttributes.Private
            | MethodAttributes.Static
            | MethodAttributes.HideBySig
            | MethodAttributes.RTSpecialName
            | MethodAttributes.SpecialName,
            this.TypeSystem.Void
        );
        
        globalsClass.Methods.Add(methodDef);

        return methodDef;
    }
        
    public MethodReference FindRuntimeMethod(string name) {
        var systemFuncsName = typeof(Runtime.SystemFunctions).FullName;

        this.systemFuncsClass ??= this.RuntimeLibrary.MainModule.Types
            .SingleOrDefault(typeDef => typeDef.FullName == systemFuncsName)
            ?? throw new TypeLoadException($"runtime library DLL did not contain {systemFuncsName}");

        var methodDef = this.systemFuncsClass.Methods
            .SingleOrDefault(methodDef => methodDef.Name == name);

        if (methodDef == null) {
            var msg = $"missing method in runtime library: {name}";
            throw new ArgumentException(msg);
        }

        return this.Module.ImportReference(methodDef);
    }
    
    public TypeReference GetSystemTypeRef(string name, bool valueType) {
        var typeRef = new TypeReference("Terapascal.Runtime",
            name,
            this.RuntimeLibrary.MainModule,
            this.RuntimeLibrary.Name,
            valueType
        );

        return this.Module.ImportReference(typeRef.Resolve());
    }

    public FieldReference GetStringLiteralRef(IR.StringID id) {
        return this.stringLitFields[id];
    }
    
    public FieldReference GetGlobalVariableRef(IR.VariableID id) {
        return this.globalVarFields[id];
    }

    public void BuildLibrary(IR.Library library) {
        // TODO: native strings
        // we have to copy the string literals into pascal strings for now

        var globals = this.GetGlobalsClass();
        var stringTypeRef = this.GetSystemTypeRef(nameof(Runtime.String), false);

        var createStringMethod = this.FindRuntimeMethod(nameof(Runtime.SystemFunctions.CreateString));

        var globalsInit = this.GetGlobalsCCtor().Body.GetILProcessor();

        foreach (var (id, stringLit) in library.Metadata.StringLiterals) {
            var fieldAttrs = FieldAttributes.Assembly | FieldAttributes.InitOnly | FieldAttributes.Static;
            var fieldDef = new FieldDefinition($"String_{id.ID}", fieldAttrs, stringTypeRef);

            globals.Fields.Add(fieldDef);

            this.stringLitFields.Add(id, fieldDef);
            
            globalsInit.Emit(OpCodes.Ldstr, stringLit);
            globalsInit.Emit(OpCodes.Call, createStringMethod);
            globalsInit.Emit(OpCodes.Stsfld, fieldDef);
        }

        foreach (var (id, varType) in library.Variables) {
            var typeRef = this.TypeBuilder.BuildTypeRef(varType);
            
            var fieldAttrs = FieldAttributes.Assembly | FieldAttributes.Static;
            var fieldDef = new FieldDefinition($"Variable_{id.ID}", fieldAttrs, typeRef);
            
            globals.Fields.Add(fieldDef);
            
            this.globalVarFields.Add(id, fieldDef);
        }
        
        foreach (var (id, typeDecl) in library.Metadata.TypeDecls) {
            switch (typeDecl) {
                case IR.DefTypeDecl { Def: var def }: {
                    switch (def) {
                        case IR.StructTypeDef { Def: var structDef }: {
                            this.TypeBuilder.BuildStructDef(id, structDef);
                            break;
                        }
                        case IR.VariantTypeDef { Def: var variantDef }: {
                            this.TypeBuilder.BuildVariantDef(id, variantDef);
                            break;
                        }
                        case IR.FunctionTypeDef { Sig: var sig }: {
                            this.TypeBuilder.BuildFunctionTypeDef(id, sig);
                            break;
                        }
                    }

                    break;
                }
            }
        }

        foreach (var (id, ifaceDecl) in library.Metadata.Interfaces) {
            if (ifaceDecl is IR.DefInterfaceDecl(var def)) {
                this.TypeBuilder.BuildInterfaceDef(id, def);
            }
        }

        this.FunctionBuilder.BuildFunctions(library);
    }

    public void Finish() {
        this.GetGlobalsCCtor().Body.GetILProcessor().Emit(OpCodes.Ret);
    }
}
