using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class TagInfo {
    [Key("class_id")]
    public required TypeDefID ClassID { get; init; }

    [Key("fields")]
    public required SortedDictionary<FieldID, IValue> Fields {
        get;
        init => field = value!.ToDictionaryNonNull();
    }
}
