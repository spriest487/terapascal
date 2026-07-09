using System.Collections;
using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public record StringPath : IReadOnlyList<string> {
    [Key("path")]
    public required IReadOnlyList<string> Path {
        get;
        init => field = value.ToArrayNonNull();
    }

    [IgnoreMember]
    public int Count => this.Path.Count;

    [IgnoreMember]
    public string Last => this.Path[^1];

    public string this[int index] => this.Path[index];

    public virtual bool Equals(StringPath? other) {
        return other is not null && other.Path.SequenceEqual(this.Path);
    }

    public IEnumerator<string> GetEnumerator() {
        return this.Path.GetEnumerator();
    }

    public override int GetHashCode() {
        var hashCode = new HashCode();
        foreach (var part in this.Path) {
            hashCode.Add(part);
        }
        return hashCode.ToHashCode();
    }

    public override string ToString() {
        return string.Join(".", this.Path);
    }

    IEnumerator IEnumerable.GetEnumerator() {
        return ((IEnumerable)this.Path).GetEnumerator();
    }

    public StringPath? GetParent() {
        if (this.Count < 2) {
            return null;
        }

        var parts = this.Path.Take(this.Path.Count - 1).ToArray();
        return new StringPath { Path = parts };
    }
}
