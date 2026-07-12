using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

[MessagePackObject(SuppressSourceGeneration = true)]
public record StringPath {
    [Key("parts")]
    public required IReadOnlyList<string> Parts {
        get;
        init => field = value.ToArrayNonNull();
    }

    [IgnoreMember]
    public int Count => this.Parts.Count;

    [IgnoreMember]
    public string Last => this.Parts[^1];

    public string this[int index] => this.Parts[index];

    public virtual bool Equals(StringPath? other) {
        return other is not null && other.Parts.SequenceEqual(this.Parts);
    }

    public IEnumerator<string> GetEnumerator() {
        return this.Parts.GetEnumerator();
    }

    public override int GetHashCode() {
        var hashCode = new HashCode();
        foreach (var part in this.Parts) {
            hashCode.Add(part);
        }
        return hashCode.ToHashCode();
    }

    public override string ToString() {
        return string.Join(".", this.Parts);
    }

    public StringPath? GetParent() {
        if (this.Count < 2) {
            return null;
        }

        var parts = this.Parts.Take(this.Parts.Count - 1).ToArray();
        return new StringPath { Parts = parts };
    }
}

[MessagePackFormatter(typeof(StringPath))]
public class StringPathFormatter : IMessagePackFormatter<StringPath> {
    public void Serialize(ref MessagePackWriter writer, StringPath? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public StringPath Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        switch (reader.NextMessagePackType) {
            case MessagePackType.Nil: {
                reader.ReadNil();
                return null!;
            }

            case MessagePackType.Map: {
                var count = reader.ReadMapHeader();
                for (var i = 0; i < count; i += 1) {
                    if (reader.ReadString() == "parts") {
                        goto case MessagePackType.Array;
                    }
                }
                throw new MessagePackSerializationException("string path missing parts key");
            }

            case MessagePackType.Array:
                var parts = MessagePackSerializer.Deserialize<string[]>(ref reader, options);
                return new StringPath { Parts = parts };

            default:
                throw new MessagePackSerializationException($"invalid type in string path: {reader.NextMessagePackType}");
        }
    }
}
