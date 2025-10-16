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

var typeBuilder = new TypeBuilder(assemblyBuilder);
var funcBuilder = new FunctionBuilder(library, typeBuilder, assemblyBuilder);

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

foreach (var (id, function) in library.Functions) {
    if (function is IR.LocalFunction localFunc) {
        funcBuilder.TranslateFunction(id, localFunc.Def);
    }
}

// remove any remaining refs to private libs that aren't explicitly referenced
// var assemblyRefs = assembly.MainModule.AssemblyReferences;
// for (var i = assemblyRefs.Count - 1; i >= 0; i -= 1) {
//     if (assemblyRefs[i] != mscorlib.Name) {
//         assemblyRefs.RemoveAt(i);
//     }
// }

await using (var output = File.Create(parsedArgs.OutputPath)) {
    assemblyBuilder.Assembly.Write(output);
}

return 0;

Stream OpenInputStream(string? libPath) {
    return libPath != null 
        ? File.OpenRead(libPath) 
        : Console.OpenStandardInput();
}
