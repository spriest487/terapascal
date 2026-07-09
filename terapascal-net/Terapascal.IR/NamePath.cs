using System.Diagnostics.CodeAnalysis;
using System.Text;
using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public sealed class NamePath : IEquatable<NamePath> {
    [Key("path")]
    public required StringPath Path {
        get;
        init => field = value ?? new StringPath { Parts = [] };
    }

    [Key("type_args")]
    public IReadOnlyList<IType>? TypeArgs { get; init; }

    [IgnoreMember, MemberNotNullWhen(true, nameof(TypeArgs))]
    public bool HasTypeArgs => this.TypeArgs is { Count: > 0 };

    [IgnoreMember]
    public string Last => this.Path.Parts[^1];

    public bool Equals(NamePath? other) {
        if (other == null) {
            return false;
        }

        if (!this.Path.Equals(other.Path)) {
            return false;
        }

        return (this.TypeArgs ?? []).SequenceEqual(other.TypeArgs ?? []);
    }

    public override bool Equals(object? obj) {
        return obj is NamePath other && this.Equals(other);
    }

    public override int GetHashCode() {
        var hashCode = new HashCode();
        hashCode.Add(this.Path);

        if (this.TypeArgs != null) {
            foreach (var type in this.TypeArgs) {
                hashCode.Add(type.GetHashCode());
            }
        }

        return hashCode.ToHashCode();
    }

    public StringPath? GetParent() {
        if (this.Path.Count == 1) {
            return null;
        }

        return new StringPath {
            Parts = this.Path.Parts.Take(this.Path.Count - 1).ToArray(),
        };
    }

    public NamePath CreateChild(string childName) {
        if (this.TypeArgs is { Count: > 0 }) {
            throw new InvalidOperationException("can't create a child of a path with type args");
        }
        
        var path = new List<string>(this.Path.Parts) { childName };

        return new NamePath {
            Path = new StringPath { Parts = path },
            TypeArgs = [],
        };
    }

    public static NamePath WithParts(params ReadOnlySpan<string> parts) {
        return new NamePath {
            Path = new StringPath { Parts = parts.ToArray() },
            TypeArgs = [],
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

        if (this.HasTypeArgs) {
            result.Append('[');
            result.AppendJoin(", ", this.TypeArgs.Select(t => t.ToPrettyString(metadata)));
            result.Append(']');
        }

        return result.ToString();
    }

    public string ToGlobalName(out string ns) {
        if (this.Path.Count > 1) {
            ns = string.Join('.', this.Path.Parts.Take(this.Path.Count - 1));
        } else {
            ns = "";
        }

        return this.Last;
    }

    public NamePath ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        var args = this.TypeArgs?
            .Select(t => t.ResolveGeneric(typeMap))
            .ToArray();

        return new NamePath {
            Path = this.Path,
            TypeArgs = args,
        };
    }
}
