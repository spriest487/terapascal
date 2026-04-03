using System.Text;
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

    public static string FormatInstructions(IReadOnlyList<IInstruction> instructions, Metadata metadata) {
        var result = new StringBuilder(instructions.Count * 16);
        FormatInstructions(instructions, metadata, result);
        return result.ToString();
    }

    public static void FormatInstructions(
        IReadOnlyList<IInstruction> instructions,
        Metadata metadata,
        StringBuilder result
    ) {
        foreach (var instruction in instructions) {
            metadata.FormatInstruction(instruction, result);
            result.AppendLine();
        }
    }
}
