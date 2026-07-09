using System.Text;
using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

[MessagePackObject]
public record StructDef {
    [Key("identity")]
    public required IStructIdentity Identity { get; init; }
    
    [Key("fields")]
    public required SortedDictionary<FieldID, StructFieldDef> Fields { get; init; }
    
    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }

    public string ToPrettyString(Metadata metadata) {
        var result = new StringBuilder();

        foreach (var (fieldID, fieldDef) in this.Fields) {
            result.Append($"{fieldID.ID}: {fieldDef.Name}");
            result.Append($"({fieldDef.Type.ToPrettyString(metadata)})");

            result.AppendLine();
        }

        return result.ToString();
    }

    public StructDef ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        var fields = new SortedDictionary<FieldID, StructFieldDef>();
        foreach (var (fieldID, fieldDef) in this.Fields) {
            var fieldType = fieldDef.Type.ResolveGeneric(typeMap);

            fields.Add(fieldID, new StructFieldDef {
                Name = fieldDef.Name,
                Type = fieldType,
            });
        }

        return new StructDef {
            Identity = this.Identity,
            Tags = this.Tags,
            Fields = fields,
        };
    }
}

[MessagePackObject]
public readonly record struct StructFieldDef {
    [Key("name")]
    public required string? Name { get; init; }

    [Key("ty")]
    public required IType Type {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
}

public interface IStructIdentity {
    bool IsValueType { get; }

    string ToPrettyString(Metadata metadata);

    DeclPath? GetDeclPath();

    IType ToDefinitionType(TypeDefID id);

}

public record RecordStructIdentity(DeclPath Name) : IStructIdentity {
    public bool IsValueType => true;

    public string ToPrettyString(Metadata metadata) {
        return this.Name.ToPrettyString(metadata);
    }

    public DeclPath GetDeclPath() {
        return this.Name;
    }

    public IType ToDefinitionType(TypeDefID id) {
        return id.ToStructType(this.Name.GetGenericArgs());
    }
}

public record ClassStructIdentity(DeclPath Name) : IStructIdentity {
    public bool IsValueType => false;

    public string ToPrettyString(Metadata metadata) {
        return this.Name.ToPrettyString(metadata);
    }

    public DeclPath GetDeclPath() {
        return this.Name;
    }

    public IType ToDefinitionType(TypeDefID id) {
        return id.ToObjectType(this.Name.GetGenericArgs());
    }
}

public record InternalStructIdentity(string InternalName) : IStructIdentity {
    public bool IsValueType => true;

    public string ToPrettyString(Metadata metadata) {
        return this.InternalName;
    }

    public DeclPath? GetDeclPath() {
        return null;
    }

    public IType ToDefinitionType(TypeDefID id) {
        return id.ToStructType([]);
    }
}

public record ClosureStructIdentity(ClosureIdentity Identity) : IStructIdentity {
    public bool IsValueType => false;

    public string ToPrettyString(Metadata metadata) {
        var result = new StringBuilder("closure object(");

        metadata.FormatFunctionRef(this.Identity.ID.ToFunctionRef([]), result);
        result.Append($": {this.Identity.Sig.ToPrettyString(metadata)})");

        return result.Append(')').ToString();
    }

    public DeclPath? GetDeclPath() {
        return null;
    }

    public IType ToDefinitionType(TypeDefID id) {
        return id.ToObjectType([]);
    }
}

[MessagePackObject]
public readonly record struct ClosureIdentity {
    [Key("sig")]
    public FunctionSig Sig { get; init; }
    
    [Key("id")]
    public FunctionID ID { get; init; }
}

public class StructIdentityFormatter : IMessagePackFormatter<IStructIdentity> {
    public void Serialize(ref MessagePackWriter writer, IStructIdentity? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IStructIdentity Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var count = reader.ReadMapHeader();
        if (count != 1) {
            throw new MessagePackSerializationException($"unexpected struct identity element count: {count}");
        }

        var key = reader.ReadString();

        switch (key) {
            case "Record": {
                var name = MessagePackSerializer.Deserialize<DeclPath>(ref reader, options);
                return new RecordStructIdentity(name);
            }

            case "Class": {
                var name = MessagePackSerializer.Deserialize<DeclPath>(ref reader, options);
                return new ClassStructIdentity(name);
            }
            
            case "Internal": {
                var name = reader.ReadString();
                return new InternalStructIdentity(name);
            }
            
            case "Closure": {
                var sig = MessagePackSerializer.Deserialize<ClosureIdentity>(ref reader, options);
                return new ClosureStructIdentity(sig);
            }
            
            default: {
                throw new MessagePackSerializationException($"illegal struct identity discriminator: {key}");
            }
        }
    }
}
