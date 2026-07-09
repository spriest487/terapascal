using MessagePack;
using MessagePack.Resolvers;

namespace Terapascal.IR;

[MessagePackObject]
public class Library {
    [Key("name")]
    public required string Name {
        get;
        init => field = value ?? "";
    }

    [Key("version")]
    public required Version Version { get; init; }

    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }
    
    [Key("references")]
    public required IReadOnlyList<string> References {
        get;
        init => field = value.ToArrayNonNull();
    }
    
    [Key("metadata")]
    public required Metadata Metadata { get; init; }

    [Key("functions")]
    public required IReadOnlyDictionary<FunctionID, IFunction> Functions {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("init")]
    public required InstructionList Initialization {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    public static IFormatterResolver FormatterResolver => CompositeResolver.Create(
        CustomCollectionsResolver.Instance,
        GeneratedMessagePackResolver.Instance
    );
}
