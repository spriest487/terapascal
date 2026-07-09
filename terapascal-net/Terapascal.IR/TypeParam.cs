using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public record TypeParam {
    [Key("name")]
    public required string Name { get; init; }

    [Key("constraint")]
    public IType? Constraint { get; init; }
}
