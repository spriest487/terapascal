using MessagePack;
using MessagePack.Resolvers;
using Mono.Cecil;
using Terapascal.CIL;
using IR = Terapascal.IR;

if (!Args.Parse(args, out var parsedArgs)) {
    return 1;
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

    IR.Library mainLib;

    if (parsedArgs.LibPath != null) {
        await using var input = File.OpenRead(parsedArgs.LibPath);
        mainLib = await LoadLibrary(input, mpOptions);
    } else {
        // expect to read from stidn
        await using var stdin = Console.OpenStandardInput();
        mainLib = await LoadLibrary(stdin, mpOptions);
    }

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

    const string rtLibPath = "Terapascal.Runtime.dll";

    using var assemblyBuilder = new AssemblyBuilder(
        mainLib.Name,
        mainLib.Version,
        moduleKind,
        rtLibPath,
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

    CopyToOutput(rtLibPath, outputDir);

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

async Task<IR.Library> LoadLibrary(Stream input, MessagePackSerializerOptions mpOptions) {
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

        IR.Library refLib;
        await using (var refLibStream = File.OpenRead(refPath)) {
            refLib = await LoadLibrary(refLibStream, mpOptions);
        }

        await AddLibraryRecursive(builder, refLib, libSearchPaths, mpOptions, loadedRefs);
    }

    if (parsedArgs.Verbose) {
        Console.WriteLine($"Building {library.Name} ({library.Version})...");
    }

    builder.AddLibrary(library);
}

void CopyToOutput(string filename, string outputDir) {
    if (!File.Exists(filename)) {
        throw new FileNotFoundException(filename);
    }

    if (Path.IsPathRooted(filename)) {
        throw new InvalidOperationException($"expected a relative path: {filename}");
    }

    var outPath = Path.Join(outputDir, filename);
    outPath = Path.GetFullPath(outPath);

    if (parsedArgs.Verbose) {
        Console.Write("copying {0} to output at {1}...", filename, outPath);
    }

    if (File.Exists(outPath)) {
        var createdDateNew = File.GetCreationTime(filename);
        var createdDateOld = File.GetCreationTime(outPath);

        if (createdDateOld >= createdDateNew) {
            // nothing to do
            if (parsedArgs.Verbose) {
                Console.WriteLine("existing file is up to date");
            }
            return;
        }
        File.Delete(outPath);
    }
    File.Copy(filename, outPath);
}
