using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;

namespace Terapascal.CIL;

public class TerapascalAssemblyBuilder {
    public const string GlobalsClassName = "Globals";

    public required AssemblyDefinition CoreLibrary { get; init; }
    public required AssemblyDefinition StandardLibrary { get; init; }
    public required AssemblyDefinition RuntimeLibrary { get; init; }
    public required AssemblyDefinition Assembly { get; init; }

    public ModuleDefinition Module => this.Assembly.MainModule;
    public TypeSystem TypeSystem => this.Assembly.MainModule.TypeSystem;

    private TypeDefinition? globalsClass;
    private TypeDefinition? systemFuncsClass;

    private Dictionary<IR.StringID, FieldDefinition> stringLitFields = new Dictionary<IR.StringID, FieldDefinition>();

    public static async Task<TerapascalAssemblyBuilder> Create(string assemblyName, Version assemblyVersion) {
        var assembly = AssemblyDefinition.CreateAssembly(
            new AssemblyNameDefinition(assemblyName, assemblyVersion),
            assemblyName,
            new ModuleParameters {
                Kind = ModuleKind.Dll,
                Runtime = TargetRuntime.Net_2_0,
            });

        var refLibPath = await SDKUtils.FindReferenceLibPath();
        var mscorlib = AssemblyDefinition.ReadAssembly(Path.Join(refLibPath, "mscorlib.dll"));
        var netstandard = AssemblyDefinition.ReadAssembly(Path.Join(refLibPath, "netstandard.dll"));

        assembly.MainModule.AssemblyReferences.Add(mscorlib.Name);
        assembly.MainModule.AssemblyReferences.Add(netstandard.Name);

        var rtLib = AssemblyDefinition.ReadAssembly("Terapascal.Runtime.dll");

        return new TerapascalAssemblyBuilder {
            Assembly = assembly,
            CoreLibrary = mscorlib,
            StandardLibrary = netstandard,
            RuntimeLibrary = rtLib,
        };
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

    public void BuildGlobals(IR.Library library) {
        // TODO: native strings
        // we have to copy the string literals into pascal strings for now

        var globals = this.GetGlobalsClass();
        var stringTypeRef = this.GetSystemTypeRef(nameof(Runtime.String), false);

        var createStringMethod = this.FindRuntimeMethod(nameof(Runtime.SystemFunctions.CreateString));

        var globalsInit = this.GetGlobalsCCtor().Body.GetILProcessor();
        
        foreach (var (id, stringLit) in library.Metadata.StringLiterals) {
            var attrs = FieldAttributes.Assembly | FieldAttributes.InitOnly | FieldAttributes.Static;
            var field = new FieldDefinition($"String_{id.ID}", attrs, stringTypeRef);

            globals.Fields.Add(field);

            this.stringLitFields.Add(id, field);
            
            globalsInit.Emit(OpCodes.Ldstr, stringLit);
            globalsInit.Emit(OpCodes.Call, createStringMethod);
            globalsInit.Emit(OpCodes.Stsfld, field);
        }
    }

    public void Finish() {
        this.GetGlobalsCCtor().Body.GetILProcessor().Emit(OpCodes.Ret);
    }
}
