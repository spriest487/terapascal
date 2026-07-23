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

[Flags]
public enum TypeFlags : ulong {
    None = 0,
    Value = 1 << 0,
    Weak = 1 << 1,
    Array = 1 << 2,
    Function = 1 << 3,
}
