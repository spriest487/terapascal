using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public readonly record struct Span {
    [Key("file")]
    public required string File { 
        get;
        init => field = value ?? "";
    }
    
    [Key("start")]
    public Location Start { get; init; }
    
    [Key("end")]
    public Location End { get; init; }
}

[MessagePackObject]
public readonly record struct Location {
    [Key("line")]
    public required ulong Line { get; init; }
    
    [Key("col")]
    public required ulong Column { get; init; }
}
