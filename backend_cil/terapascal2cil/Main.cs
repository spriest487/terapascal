using MessagePack;
using MessagePack.Resolvers;
using Mono.Cecil;
using Terapascal.CIL;
using IR = Terapascal.IR;

if (!Args.Parse(args, out var parsedArgs)) {
    return 1;
}

var mpOptions = MessagePackSerializerOptions.Standard.WithResolver(CompositeResolver.Create([
    CustomCollectionsResolver.Instance,
    GeneratedMessagePackResolver.Instance,
    StandardResolver.Instance,
]));

IR.Library library;
await using (var input = OpenInputStream(parsedArgs.LibPath)) {
    library = await MessagePackSerializer.DeserializeAsync<IR.Library>(input, mpOptions);
}

if (!Enum.TryParse(parsedArgs.ModuleKind, out ModuleKind moduleKind)) {
    moduleKind = ModuleKind.Dll;
}

if (parsedArgs.Verbose) {
    Console.WriteLine($"generating assembly: {library.Name} {library.Version} ({moduleKind})");
}

var refLibPath = await SDKUtils.FindReferenceLibPath(parsedArgs.SDKVersion, parsedArgs.Verbose);

var targetVersion = parsedArgs.TargetRuntimeVersion;
if (targetVersion == null) {
    targetVersion = await SDKUtils.FindTargetRuntimeVersion(parsedArgs.Verbose);
}

using (var assemblyBuilder = new AssemblyBuilder(library.Name,
    library.Version,
    moduleKind,
    "Terapascal.Runtime.dll",
    refLibPath)
) {
    assemblyBuilder.AddLibrary(library);
    assemblyBuilder.Finish();

    var outputPath = Path.GetFullPath(parsedArgs.OutputPath);

    var outputDir = Path.GetDirectoryName(outputPath);
    if (outputDir == null) {
        throw new DirectoryNotFoundException($"couldn't find output directory from path {outputPath}");
    }

    Directory.CreateDirectory(outputDir);

    await using (var output = File.Create(outputPath)) {
        assemblyBuilder.Assembly.Write(output);
    }

    if (parsedArgs.Verbose) {
        Console.WriteLine($"output assembly written to {outputPath}");
    }

    // copy the runtime DLL next to the output file
    if (parsedArgs.Verbose) {
        Console.WriteLine($"output directory: {outputDir}");
    }

    var rtOutputPath = Path.Join(outputDir, assemblyBuilder.RuntimeLibrary.Name.Name + ".dll");
    rtOutputPath = Path.GetFullPath(rtOutputPath);

    assemblyBuilder.RuntimeLibrary.Write(rtOutputPath);

    if (parsedArgs.Verbose) {
        Console.WriteLine($"RT assembly written to {rtOutputPath}");
    }

    if (assemblyBuilder.IsExecutable) {
        // for now assume all DLLs are runnable and output a runtime config file too
        var assemblyFilename = Path.GetFileNameWithoutExtension(outputPath);
        var runtimeConfigPath = Path.Join(outputDir, $"{assemblyFilename}.runtimeconfig.json");

        await SDKUtils.CreateTemplatedFile("template.runtimeconfig.json", runtimeConfigPath,
            new KeyValuePair<string, string>("$TARGET_VERSION", targetVersion));

        if (parsedArgs.Verbose) {
            Console.WriteLine($"runtimeconfig file written to {runtimeConfigPath}");
        }
    }
}

return 0;

Stream OpenInputStream(string? libPath) {
    return libPath != null
        ? File.OpenRead(libPath)
        : Console.OpenStandardInput();
}
