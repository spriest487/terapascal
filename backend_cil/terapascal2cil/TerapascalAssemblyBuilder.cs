using Mono.Cecil;

namespace Terapascal.CIL;

public class TerapascalAssemblyBuilder {
    public AssemblyDefinition CoreLibrary { get; private init; }
    public AssemblyDefinition StandardLibrary { get; private init; }
    public AssemblyDefinition RuntimeLibrary { get; private init; }
    
    public AssemblyDefinition Assembly { get; private init; }

    public ModuleDefinition Module => this.Assembly.MainModule;
    public TypeSystem TypeSystem => this.Assembly.MainModule.TypeSystem;

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
}
