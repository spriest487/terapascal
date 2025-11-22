using System.Text;
using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public sealed class NamePath : IEquatable<NamePath> {
    [Key("path")]
    public required IReadOnlyList<string> Path {
        get;
        init {
            var parts = value!.ToArrayNonNull();
            if (parts.Length < 1) {
                throw new ArgumentException("empty path", nameof(value));
            }
            field = parts;
        }
    }

    [Key("type_args")]
    public required IReadOnlyList<IType>? TypeArgs {
        get;
        init => field = value!.ToArrayNonNull();
    }

    public bool Equals(NamePath? other) {
        if (other == null) {
            return false;
        }

        if (!this.Path.SequenceEqual(other.Path)) {
            return false;
        }

        return (this.TypeArgs ?? []).SequenceEqual(other.TypeArgs ?? []);
    }

    public override bool Equals(object? obj) {
        return obj is NamePath other && this.Equals(other);
    }

    public override int GetHashCode() {
        var hashCode = 123456;
        foreach (var part in this.Path) {
            hashCode ^= part.GetHashCode();
        }

        if (this.TypeArgs != null) {
            foreach (var type in this.TypeArgs) {
                hashCode ^= type.GetHashCode();
            }
        }

        return hashCode;
    }

    public NamePath? GetParent() {
        if (this.Path.Count == 1) {
            return null;
        }

        return new NamePath {
            Path = this.Path.Take(this.Path.Count - 1).ToList(),
            TypeArgs = null,
        };
    }

    public NamePath CreateChild(string childName) {
        if (this.TypeArgs != null && this.TypeArgs.Count > 0) {
            throw new InvalidOperationException("can't create a child of a path with type args");
        }
        
        var path = new List<string>(this.Path) { childName };

        return new NamePath {
            Path = path,
            TypeArgs = null,
        };
    }

    public static NamePath WithParts(params ReadOnlySpan<string> parts) {
        return new NamePath {
            Path = parts.ToArray(),
            TypeArgs = null,
        };
    }

    public override string ToString() {
        var result = new StringBuilder();
        result.AppendJoin(".", this.Path);

        if (this.TypeArgs is { Count: > 0 }) {
            result.Append('[');
            result.AppendJoin(", ", this.TypeArgs);
            result.Append(']');
        }

        return result.ToString();
    }
    
    public string ToPrettyString(Metadata metadata) {
        var result = new StringBuilder();
        result.AppendJoin(".", this.Path);

        if (this.TypeArgs != null) {
            result.Append('[');
            result.AppendJoin(", ", this.TypeArgs.Select(t => t.ToPrettyString(metadata)));
            result.Append(']');
        }

        return result.ToString();
    }
}
