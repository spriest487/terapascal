using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class VariableInfo {
    [Key("name")]
    public StringPath? Name { get; init; }
    
    [Key("value_type")]
    public required IType Type { get; init; }

    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }
    
    [Key("visibility")]
    public required Visibility Visibility { get; init; }
}
