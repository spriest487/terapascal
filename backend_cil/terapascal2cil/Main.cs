using MessagePack;
using MessagePack.Resolvers;
using Terapascal.CIL;
using IR = Terapascal.IR;

if (!Args.Parse(args, out var parsedArgs)) {
    return 1;
}

var mpOptions = MessagePackSerializerOptions.Standard.WithResolver(CompositeResolver.Create([
    StandardResolver.Instance,
    GeneratedMessagePackResolver.Instance,
    CustomCollectionsResolver.Instance,
]));

IR.Library library;
await using (var input = OpenInputStream(parsedArgs.LibPath)) {
    library = await MessagePackSerializer.DeserializeAsync<IR.Library>(input, mpOptions);
}

var assemblyName = parsedArgs.AssemblyName;
var assemblyVersion = parsedArgs.Version ?? new Version(1, 0, 0, 0);

var assemblyBuilder = await TerapascalAssemblyBuilder.Create(assemblyName, assemblyVersion);

assemblyBuilder.BuildGlobals(library);

var typeBuilder = new TypeBuilder(assemblyBuilder);
var funcBuilder = new FunctionBuilder(typeBuilder, assemblyBuilder);

foreach (var (id, typeDecl) in library.Metadata.TypeDecls) {
    switch (typeDecl) {
        case IR.DefTypeDecl { Def: var def }: {
            switch (def) {
                case IR.StructTypeDef { Def: var structDef }: {
                    typeBuilder.BuildStructDef(id, structDef);
                    break;
                }
                case IR.VariantTypeDef { Def: var variantDef }: {
                    typeBuilder.BuildVariantDef(id, variantDef);
                    break;
                }
                case IR.FunctionTypeDef { Sig: var sig }: {
                    typeBuilder.BuildFunctionTypeDef(id, sig);
                    break;
                }
            }

            break;
        }
    }
}

foreach (var (id, ifaceDecl) in library.Metadata.Interfaces) {
    if (ifaceDecl is IR.DefInterfaceDecl(var def)) {
        typeBuilder.BuildInterfaceDef(id, def);
    }
}

funcBuilder.BuildFunctions(library);

// remove any remaining refs to private libs that aren't explicitly referenced
// var assemblyRefs = assembly.MainModule.AssemblyReferences;
// for (var i = assemblyRefs.Count - 1; i >= 0; i -= 1) {
//     if (assemblyRefs[i] != mscorlib.Name) {
//         assemblyRefs.RemoveAt(i);
//     }
// }

assemblyBuilder.Finish();

var outputPath = Path.GetFullPath(parsedArgs.OutputPath);
await using (var output = File.Create(outputPath)) {
    assemblyBuilder.Assembly.Write(output);
}
Console.WriteLine($"output assembly written to {outputPath}");

// copy the runtime DLL next to the output file
if (Path.GetDirectoryName(parsedArgs.OutputPath) is {} outputDir) {
    var rtOutputPath = Path.Join(outputDir, assemblyBuilder.RuntimeLibrary.Name.Name + ".dll");
    rtOutputPath = Path.GetFullPath(rtOutputPath);

    assemblyBuilder.RuntimeLibrary.Write(rtOutputPath);
    
    Console.WriteLine($"RT assembly written to {rtOutputPath}");
}

return 0;

Stream OpenInputStream(string? libPath) {
    return libPath != null 
        ? File.OpenRead(libPath) 
        : Console.OpenStandardInput();
}
