using System.Reflection;
using System.Reflection.Emit;
using MessagePack;
using MessagePack.Resolvers;
using Terapascal.CIL;
using Terapascal.IR;

if (!Args.Parse(args, out var parsedArgs)) {
    return 1;
}

var name = new AssemblyName(parsedArgs.AssemblyName);
var coreAssembly = typeof(object).Assembly;

var builder = new PersistedAssemblyBuilder(name, coreAssembly);

using var libFile = new MemoryStream();

await using (var input = OpenInputStream(parsedArgs.LibPath)) {
    await input.CopyToAsync(libFile);
    libFile.Position = 0;
}

var messagePack = MessagePackSerializer.Typeless.Deserialize(libFile);
libFile.Position = 0;

var mpOptions = MessagePackSerializerOptions.Standard.WithResolver(CompositeResolver.Create([
    StandardResolver.Instance,
    GeneratedMessagePackResolver.Instance,
    CustomCollectionsResolver.Instance,
]));

var library = await MessagePackSerializer.DeserializeAsync<Library>(libFile, mpOptions);

Console.WriteLine($"done: {libFile.Length}");

return 0;

Stream OpenInputStream(string? libPath) {
    if (libPath != null) {
        return File.OpenRead(libPath);
    }

    return Console.OpenStandardInput();
}
