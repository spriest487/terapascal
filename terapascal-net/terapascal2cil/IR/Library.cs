using MessagePack;

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
    
    [Key("references")]
    public required IReadOnlyList<string> References {
        get;
        init => field = value!.ToArrayNonNull();
    }
    
    [Key("metadata")]
    public required Metadata Metadata { get; set; }

    [Key("functions")]
    public required IReadOnlyDictionary<FunctionID, IFunction> Functions {
        get;
        init => field = value!.ToDictionaryNonNull();
    }

    [Key("static_closures")]
    public required IReadOnlyList<StaticClosure> StaticClosures {
        get;
        init => field = value!.ToArrayNonNull();
    }

    [Key("init")]
    public required InstructionList Initialization {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
}
