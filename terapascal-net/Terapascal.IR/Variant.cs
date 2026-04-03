using System.Text;
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
    
    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }

    public string ToPrettyString(Metadata metadata) {
        var result = new StringBuilder();

        for (var caseIndex = 0; caseIndex < this.Cases.Count; caseIndex += 1) {
            var caseDef = this.Cases[caseIndex];
            
            result.Append($"{caseIndex}: {caseDef.Name} = ");
            metadata.FormatValue(caseDef.Tag, result);
            
            if (caseDef.Type != null) {
                result.Append($"({caseDef.Type.ToPrettyString(metadata)})");
            }

            result.AppendLine();
        }

        return result.ToString();
    }
}

[MessagePackObject]
public readonly record struct VariantCase {
    [Key("name")]
    public required string Name {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("tag")]
    public required IValue Tag {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("ty")]
    [MessagePackFormatter(typeof(NullableTypeFormatter))]
    public required IType? Type { get; init; }
}
