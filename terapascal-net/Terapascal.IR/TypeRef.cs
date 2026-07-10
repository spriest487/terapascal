using System.Text;
using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public record TypeRef {
    [Key("def_id")]
    public TypeDefID DefID { get; init; }

    [Key("args")]
    public IReadOnlyList<IType>? Args { get; init; }

    [IgnoreMember]
    public bool HasArgs => this.Args is { Count: > 0 };

    [IgnoreMember]
    public bool ContainsGenericParams => this.Args != null
        && this.Args.Any(t => t.ContainsGenericParams);

    public TypeRef ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        var args = this.Args?
                .Select(a => a.ResolveGeneric(typeMap))
                .ToArray()
            ?? [];

        return new TypeRef {
            DefID = this.DefID,
            Args = args,
        };
    }

    public IType ToStructType() {
        return new StructType(this);
    }

    public IType ToVariantType() {
        return new VariantType(this);
    }

    public IObjectID ToClassObjectID() {
        return new ClassObjectID(this);
    }

    public override int GetHashCode() {
        var hashCode = new HashCode();
        hashCode.Add(this.DefID);

        if (this.Args != null) {
            foreach (var typeArg in this.Args) {
                hashCode.Add(typeArg);
            }
        }

        return hashCode.ToHashCode();
    }

    public string ToString(IMetadataSource? metadata) {
        var result = new StringBuilder($"{nameof(TypeRef)}(");
        result.Append(this.DefID);
        NamePath.FormatTypeArgsList(this.Args, metadata, result);

        result.Append(')');
        return result.ToString();
    }

    public override string ToString() {
        return this.ToString(null);
    }
}

[MessagePackObject]
public record InterfaceRef {
    [Key("def_id")]
    public InterfaceID DefID { get; init; }

    [Key("args")]
    public IReadOnlyList<IType>? Args { get; init; }

    [IgnoreMember]
    public bool ContainsGenericParams => this.Args != null && this.Args.Any(t => t.ContainsGenericParams);

    public InterfaceRef ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        if (this.Args == null || this.Args.Count == 0) {
            return this;
        }

        return this with {
            Args = this.Args?
                .Select(a => a.ResolveGeneric(typeMap))
                .ToArray(),
        };
    }

    public IObjectID ToObjectID() {
        return new InterfaceObjectID(this);
    }

    public override int GetHashCode() {
        var hashCode = new HashCode();
        hashCode.Add(this.DefID);

        if (this.Args != null) {
            foreach (var typeArg in this.Args) {
                hashCode.Add(typeArg);
            }
        }

        return hashCode.ToHashCode();
    }

    public string ToString(IMetadataSource? metadata) {
        var result = new StringBuilder($"{nameof(InterfaceRef)}(");
        result.Append(this.DefID);
        NamePath.FormatTypeArgsList(this.Args, metadata, result);

        result.Append(')');
        return result.ToString();
    }

    public override string ToString() {
        return this.ToString(null);
    }
}
