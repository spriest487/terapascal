using MessagePack;
using MessagePack.Resolvers;
using Mono.Cecil;
using Terapascal.CIL;
using IR = Terapascal.IR;

if (!Args.Parse(args, out var parsedArgs)) {
    return 1;
}

if (parsedArgs.LibPath == null) {
    return 0;
}

try {
    var refLibPath = await SDKUtils.FindReferenceLibPath(parsedArgs.SDKVersion, parsedArgs.Verbose);

    var targetVersion = parsedArgs.TargetRuntimeVersion;
    if (targetVersion == null) {
        targetVersion = await SDKUtils.FindTargetRuntimeVersion(parsedArgs.Verbose);
    }

    var mpOptions = MessagePackSerializerOptions.Standard.WithResolver(CompositeResolver.Create([
        IR.Library.FormatterResolver,
        StandardResolver.Instance,
    ]));

    if (!Enum.TryParse(parsedArgs.ModuleKind, out ModuleKind moduleKind)) {
        moduleKind = ModuleKind.Dll;
    }

    var mainLib = await LoadLibrary(parsedArgs.LibPath, mpOptions);

    if (parsedArgs.Verbose) {
        Console.WriteLine($"generating assembly: {mainLib.Name} {mainLib.Version} ({moduleKind})");
    }

    var libSearchPaths = new List<string>();

    var mainLibDir = Path.GetDirectoryName(parsedArgs.LibPath);
    if (mainLibDir != null) {
        libSearchPaths.Add(mainLibDir);
    }

    var libEnvDir = Environment.GetEnvironmentVariable("TERAPASCAL_LIB");
    if (libEnvDir != null) {
        libSearchPaths.Add(libEnvDir);
    }

    using var assemblyBuilder = new AssemblyBuilder(
        mainLib.Name,
        mainLib.Version,
        moduleKind,
        "Terapascal.Runtime.dll",
        refLibPath);

    var loadedRefs = new HashSet<string>();
    await AddLibraryRecursive(assemblyBuilder, mainLib, libSearchPaths, mpOptions, loadedRefs);

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
} catch (Exception e) {
    Console.Error.WriteLine(e);
    return 1;
}

return 0;

Stream OpenInputStream(string? libPath) {
    return libPath != null
        ? File.OpenRead(libPath)
        : Console.OpenStandardInput();
}

async Task<IR.Library> LoadLibrary(string path, MessagePackSerializerOptions mpOptions) {
    await using var input = OpenInputStream(path);
    return await MessagePackSerializer.DeserializeAsync<IR.Library>(input, mpOptions);
}

string FindLibPath(string libName, IReadOnlyList<string> libSearchPaths) {
    var filename = libName + ".lib";
    foreach (var searchPath in libSearchPaths) {
        var fullPath = Path.Combine(searchPath, filename);

        if (File.Exists(fullPath)) {
            return fullPath;
        }
    }

    throw new FileNotFoundException(filename);
}

async Task AddLibraryRecursive(
    AssemblyBuilder builder,
    IR.Library library,
    IReadOnlyList<string> libSearchPaths,
    MessagePackSerializerOptions mpOptions,
    HashSet<string> loadedRefs
) {
    foreach (var refName in library.References) {
        if (!loadedRefs.Add(refName)) {
            continue;
        }

        var refPath = FindLibPath(refName, libSearchPaths);
        if (parsedArgs.Verbose) {
            Console.WriteLine($"Loading referenced library {refName} at {refPath}");
        }

        var refLib = await LoadLibrary(refPath, mpOptions);

        await AddLibraryRecursive(builder, refLib, libSearchPaths, mpOptions, loadedRefs);
    }


    Console.WriteLine($"Building {library.Name} ({library.Version})...");
    builder.AddLibrary(library);
}
