using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public enum Visibility {
    Internal = 0,
    Public = 1,
}

public class VisibilityFormatter : IMessagePackFormatter<Visibility> {
    public void Serialize(
        ref MessagePackWriter writer,
        Visibility value,
        MessagePackSerializerOptions options
    ) {
        throw new NotImplementedException();
    }

    public Visibility Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.NextMessagePackType != MessagePackType.String) {
            var count = reader.ReadMapHeader();
            if (count != 1) {
                throw new MessagePackSerializationException($"unexpected type element count: {count}");
            }
        }

        return reader.ReadString() switch {
            "Internal" => Visibility.Internal,
            "Public" => Visibility.Public,
            _ => throw new MessagePackSerializationException($"unexpected value: {reader.NextMessagePackType}"),
        };
    }
}
