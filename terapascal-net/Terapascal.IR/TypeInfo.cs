using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class TypeInfo {
    [Key("name")]
    public StringID? Name { get; init; }

    [Key("methods")]
    public required IReadOnlyList<MethodInfo> Methods {
        get;
        init => field = value.ToArrayNonNull();
    }

    [Key("flags")]
    public ulong Flags { get; init; }
}
