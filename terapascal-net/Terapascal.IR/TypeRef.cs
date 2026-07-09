using MessagePack;

namespace Terapascal.IR;

[MessagePackObject]
public class TypeRef {
    [Key("def_id")]
    public TypeDefID DefID { get; init; }

    [Key("args")]
    public IReadOnlyList<IType>? Args { get; init; }

    [IgnoreMember]
    public bool HasArgs => this.Args is { Count: > 0 };

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
        return new StructType(this);
    }

    public IObjectID ToClassObjectID() {
        return new ClassObjectID(this);
    }
}

[MessagePackObject]
public class InterfaceRef {
    [Key("def_id")]
    public InterfaceID DefID { get; init; }

    [Key("args")]
    public IReadOnlyList<IType>? Args { get; init; }

    public InterfaceRef ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        if (this.Args == null || this.Args.Count == 0) {
            return this;
        }

        return new InterfaceRef {
            DefID = this.DefID,
            Args = this.Args?
                .Select(a => a.ResolveGeneric(typeMap))
                .ToArray(),
        };
    }

    public IObjectID ToObjectID() {
        return new InterfaceObjectID(this);
    }
}
