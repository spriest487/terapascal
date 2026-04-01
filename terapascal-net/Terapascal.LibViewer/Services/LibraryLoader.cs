using System.IO;
using System.Threading;
using System.Threading.Tasks;
using MessagePack;
using MessagePack.Resolvers;

namespace Terapascal.LibViewer.Services;

public class LibraryLoader {
    public IR.Library? CurrentLibrary { get; private set; }

    public async Task<IR.Library> LoadLibrary(string path, CancellationToken cancellationToken) {
        await using var file = File.OpenRead(path);
        cancellationToken.ThrowIfCancellationRequested();

        var resolver = CompositeResolver.Create(
            IR.Library.FormatterResolver, 
            StandardResolver.Instance
        );

        var serializerOpts = MessagePackSerializerOptions.Standard.WithResolver(resolver);

        var lib = await MessagePackSerializer.DeserializeAsync<IR.Library>(file, serializerOpts, cancellationToken);
        this.CurrentLibrary = lib;
        return lib;
    }
}
