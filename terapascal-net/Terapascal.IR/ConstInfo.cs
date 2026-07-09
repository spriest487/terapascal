using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class ConstInfo {
    [Key("name")]
    public required StringPath Name { get; init; }
    
    [Key("value")]
    public required IValue Value { get; init; }
    
    [Key("value_type")]
    public required IType ValueType { get; init; }

    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }
    
    [Key("visibility")]
    public required Visibility Visibility { get; init; }
}
