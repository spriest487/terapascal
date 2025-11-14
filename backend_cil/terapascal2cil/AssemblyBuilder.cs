using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

namespace Terapascal.CIL;

public class AssemblyBuilder : IDisposable {
    public const string GlobalsClassName = "Globals";

    public IReadOnlyList<IR.Library> IRLibraries => this.libraries;

    public AssemblyDefinition StandardLibrary { get; }
    public AssemblyDefinition RuntimeLibrary { get; }
    public AssemblyDefinition Assembly { get; }
    
    public TypeBuilder TypeBuilder { get; }
    public FunctionBuilder FunctionBuilder { get; }

    private readonly List<IR.Library> libraries;

    public ModuleDefinition Module => this.Assembly.MainModule;
    public TypeSystem TypeSystem => this.Assembly.MainModule.TypeSystem;

    private TypeDefinition? globalsClass;
    private TypeDefinition? systemFuncsClass;

    private readonly Dictionary<IR.StringID, FieldDefinition> stringLitFields;
    private readonly Dictionary<IR.VariableID, FieldDefinition> globalVarFields;

    private readonly Dictionary<IR.StaticClosureID, FieldDefinition> staticClosureFields;

    private readonly List<MethodDefinition> initMethods;

    public AssemblyBuilder(
        string assemblyName,
        Version assemblyVersion,
        ModuleKind moduleKind,
        string rtLibPath,
        string refLibPath
    ) {
        this.libraries = new List<IR.Library>(1);

        this.Assembly = AssemblyDefinition.CreateAssembly(
            new AssemblyNameDefinition(assemblyName, assemblyVersion),
            assemblyName,
            new ModuleParameters {
                Kind = moduleKind,
                Runtime = TargetRuntime.Net_4_0,
            });

        this.StandardLibrary = AssemblyDefinition.ReadAssembly(Path.Join(refLibPath, "netstandard.dll"));
        this.RuntimeLibrary = AssemblyDefinition.ReadAssembly(rtLibPath);

        this.TypeBuilder = new TypeBuilder(this);
        this.FunctionBuilder = new FunctionBuilder(this);

        this.stringLitFields = new Dictionary<IR.StringID, FieldDefinition>();
        this.globalVarFields = new Dictionary<IR.VariableID, FieldDefinition>();

        this.staticClosureFields = new Dictionary<IR.StaticClosureID, FieldDefinition>();

        this.initMethods = new List<MethodDefinition>(1);
    }

    public void Dispose() {
        this.Assembly.Dispose();
        
        this.RuntimeLibrary.Dispose();

        this.StandardLibrary.Dispose();
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
            TypeAttributes.Sealed | TypeAttributes.Class | TypeAttributes.Public,
            objectType
        );
        
        this.Module.Types.Add(this.globalsClass);

        return this.globalsClass;
    }

    private MethodDefinition GetInitMethodBuilder() {
        var globalsClass = this.GetGlobalsClass();

        if (this.Module.Kind is ModuleKind.Windows or ModuleKind.Console) {
            var mainMethod = globalsClass.Methods.FirstOrDefault(m => m.Name == "Main");
            if (mainMethod != null) {
                return mainMethod;
            }

            var attrs = MethodAttributes.Public | MethodAttributes.Static;

            var methodDef = new MethodDefinition("Main", attrs, this.TypeSystem.Void);

            globalsClass.Methods.Add(methodDef);
            return methodDef;
        } else {
            var cctor = globalsClass.GetStaticConstructor();
            if (cctor != null) {
                return cctor;
            }

            var attrs = MethodAttributes.Assembly
                | MethodAttributes.Static
                | MethodAttributes.HideBySig
                | MethodAttributes.RTSpecialName
                | MethodAttributes.SpecialName;

            var methodDef = new MethodDefinition(
                ".cctor",
                attrs,
                this.TypeSystem.Void
            );
        
            globalsClass.Methods.Add(methodDef);

            return methodDef;
        }
    }
        
    public MethodReference FindRuntimeFunction(string name) {
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

    
    public TypeReference GetRuntimeTypeRef(string name, bool valueType) {
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

    public void AddLibrary(IR.Library library) {
        this.libraries.Add(library);
        
        // TODO: native strings
        // we have to copy the string literals into pascal strings for now

        var globals = this.GetGlobalsClass();
        var stringTypeRef = this.GetRuntimeTypeRef(nameof(Runtime.String), false);

        var createStringMethod = this.FindRuntimeFunction(nameof(Runtime.SystemFunctions.CreateString));

        var globalsInit = this.GetInitMethodBuilder().Body.GetILProcessor();

        foreach (var (id, stringLit) in library.Metadata.StringLiterals) {
            var fieldAttrs = FieldAttributes.Assembly | FieldAttributes.InitOnly | FieldAttributes.Static;
            var fieldDef = new FieldDefinition($"String_{id.ID}", fieldAttrs, stringTypeRef);

            globals.Fields.Add(fieldDef);

            this.stringLitFields.Add(id, fieldDef);

            globalsInit.Emit(OpCodes.Ldstr, stringLit);
            globalsInit.Emit(OpCodes.Ldc_I4, 1);
            globalsInit.Emit(OpCodes.Call, createStringMethod);
            globalsInit.Emit(OpCodes.Stsfld, fieldDef);
        }

        foreach (var (id, varType) in library.Variables) {
            var typeRef = this.TypeBuilder.BuildTypeRef(varType, library);
            
            var fieldAttrs = FieldAttributes.Assembly | FieldAttributes.Static;
            var fieldDef = new FieldDefinition($"Variable_{id.ID}", fieldAttrs, typeRef);
            
            globals.Fields.Add(fieldDef);
            
            this.globalVarFields.Add(id, fieldDef);
        }
        
        foreach (var (id, typeDecl) in library.Metadata.TypeDecls) {
            switch (typeDecl) {
                case IR.DefTypeDecl { Def: var def }: {
                    if (id == IR.TypeDefID.String 
                        || id == IR.TypeDefID.TypeInfo 
                        || id == IR.TypeDefID.MethodInfo 
                        || id == IR.TypeDefID.FunctionInfo) {
                        // skip classes defined in the runtime DLL
                        continue;
                    }
                    
                    switch (def) {
                        case IR.StructTypeDef { Def: var structDef }: {
                            this.TypeBuilder.BuildStructDef(id, structDef, library);
                            break;
                        }
                        case IR.VariantTypeDef { Def: var variantDef }: {
                            this.TypeBuilder.BuildVariantDef(id, variantDef, library);
                            break;
                        }
                        case IR.FunctionTypeDef { Sig: var sig }: {
                            this.TypeBuilder.BuildFunctionTypeDef(id, sig, library);
                            break;
                        }
                    }

                    break;
                }
            }
        }

        foreach (var staticClosure in library.StaticClosures) {
            var fieldAttrs = FieldAttributes.Assembly | FieldAttributes.Static;
            var fieldName = $"StaticClosure_{staticClosure.ID.ID}";
            var fieldDef = new FieldDefinition(fieldName, fieldAttrs, this.TypeBuilder.ClosureBaseType);
            
            globals.Fields.Add(fieldDef);
            
            this.staticClosureFields.Add(staticClosure.ID, fieldDef);
        }

        foreach (var (id, ifaceDecl) in library.Metadata.Interfaces) {
            if (ifaceDecl is IR.DefInterfaceDecl(var ifaceDef)) {
                this.TypeBuilder.BuildInterfaceDef(id, ifaceDef, library);
            }
        }

        this.FunctionBuilder.BuildFunctions(library);

        if (library.Initialization.Count > 0) {
            var initAttrs = MethodAttributes.Private | MethodAttributes.Static;
            var initMethod = new MethodDefinition($"Init_{this.initMethods.Count}", initAttrs, this.TypeSystem.Void);
            
            this.initMethods.Add(initMethod);
            globals.Methods.Add(initMethod);

            var initBuilder = new InstructionBuilder(this, library, initMethod);
            initBuilder.AddInstructions(library.Initialization);
            initBuilder.Finish();
            
            globalsInit.Emit(OpCodes.Call, initMethod);
        }
    }

    public IR.FunctionSig? GetFunctionPointerType(IR.TypeDefID id) {
        foreach (var lib in this.libraries) {
            if (!lib.Metadata.TypeDecls.TryGetValue(id, out var decl)) {
                continue;
            }

            if (decl is IR.DefTypeDecl(IR.FunctionTypeDef(var sig))) {
                return sig;
            } 
        }

        return null;
    }

    public FieldDefinition GetStaticClosureFieldRef(IR.StaticClosureID id) {
        return this.staticClosureFields[id];
    }

    public bool IsClosureStruct(IR.TypeDefID closureStructID) {
        return this.libraries.Any(lib => lib.Metadata.Closures
            .Any(funcClosures => funcClosures.Value.Contains(closureStructID)));
    }

    public void Finish() {
        var initFunction = this.GetInitMethodBuilder();
        var globalsInit = initFunction.Body.GetILProcessor();

        globalsInit.Emit(OpCodes.Ret);

        if (this.Assembly.MainModule.Kind is ModuleKind.Console or ModuleKind.Windows) {
            this.Assembly.EntryPoint = initFunction;
        }
    }
}
