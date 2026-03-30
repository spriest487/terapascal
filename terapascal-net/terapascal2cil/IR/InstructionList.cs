using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class InstructionList {
    [Key("instructions")]
    public required IReadOnlyList<IInstruction> Instructions {
        get;
        init => field = value.ToArrayNonNull();
    }
    
    [Key("sources")]
    public required IReadOnlyList<Span?> Sources {
        get;
        init => field = value ?? Array.Empty<Span?>();
    }
}
