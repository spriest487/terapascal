using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public record NamePath {
    [Key("path")]
    public required IReadOnlyList<string> Path {
        get;
        init => field = value!.ToArrayNonNull();
    }

    [Key("type_args")]
    public required IReadOnlyList<IType?>? TypeArgs {
        get;
        init => field = value!.ToArrayNonNull();
    }
}
