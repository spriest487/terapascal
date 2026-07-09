using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public record TypeParam {
    [Key("name")]
    public required string Name { get; init; }

    [Key("constraint")]
    [MessagePackFormatter(typeof(NullableTypeFormatter))]
    public IType? Constraint { get; init; }
}
