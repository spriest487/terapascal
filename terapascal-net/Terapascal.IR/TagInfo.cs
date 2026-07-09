using System.Text;
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

    public void ToPrettyString(IMetadataSource metadata, StringBuilder result) {
        result.Append('[');
        result.Append(this.ClassID.ToObjectType([]).ToString(metadata));
        
        if (this.Fields.Count > 0) {
            result.Append('(');

            if (!metadata.FindStructDef(this.ClassID, out var classDef)) {
                classDef = null;
            }
            
            var fieldNum = 0;
            foreach (var (fieldID, fieldVal) in this.Fields) {
                if (fieldNum > 0) {
                    result.Append("; ");
                }
                
                if (classDef != null 
                    && classDef.Fields.TryGetValue(fieldID, out var fieldDef)
                    && fieldDef.Name != null
                ) {
                    result.Append(fieldDef.Name);
                } else {
                    result.Append(fieldID.ID);
                }

                result.Append(": ");
                metadata.FormatValue(fieldVal, result);
                
                fieldNum += 1;
            }

            result.Append(')');
        }

        result.Append(']');
    }
}
