using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public record VariantDef {
    [Key("name")]
    public required NamePath Name {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("tag_type")]
    public required IType TagType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
    
    [Key("cases")]
    public required IReadOnlyList<VariantCase> Cases { 
        get;
        init => field = value!.ToArrayNonNull();
    }
}

[MessagePackObject]
public readonly record struct VariantCase {
    [Key("name")]
    public required string Name {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("ty")]
    public required IType? Type { get; init; }
}
