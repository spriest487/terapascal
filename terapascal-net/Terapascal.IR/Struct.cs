using System.Text;
using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public enum StructLayout {
    Default,
    Packed,
}

public class StructLayoutFormatter : IMessagePackFormatter<StructLayout> {
    public void Serialize(ref MessagePackWriter writer, StructLayout value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public StructLayout Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var name = reader.ReadString();
        switch (name) {
            case "Default": {
                return StructLayout.Default;
            }

            case "Packed": {
                return StructLayout.Packed;
            }

            default: {
                throw new MessagePackSerializationException("invalid value for StructLayout enum: {name}");
            }
        }
    }
}

[MessagePackObject]
public record StructDef {
    [Key("identity")]
    public required IStructIdentity Identity { get; init; }

    [Key("visibility")]
    public required Visibility Visibility { get; init; }
    
    [Key("fields")]
    public required SortedDictionary<FieldID, StructFieldDef> Fields { get; init; }
    
    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }

    [Key("layout")]
    public required StructLayout Layout { get; init; }

    public string ToPrettyString(Metadata metadata) {
        var result = new StringBuilder();

        foreach (var (fieldID, fieldDef) in this.Fields) {
            result.Append($"{fieldID.ID}: {fieldDef.Name}");
            result.Append($"({fieldDef.Type.ToString(metadata)})");

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
            Visibility = this.Visibility,
            Layout = this.Layout,
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

    string ToString(IMetadataSource? metadata);

    DeclPath? GetDeclPath();

    IType ToDefinitionType(TypeDefID id);

}

public record RecordStructIdentity(DeclPath Name) : IStructIdentity {
    public bool IsValueType => true;

    public string ToString(IMetadataSource? metadata) {
        return this.Name.ToString(metadata);
    }

    public override string ToString() {
        return this.ToString(null);
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

    public string ToString(IMetadataSource? metadata) {
        return this.Name.ToString(metadata);
    }

    public override string ToString() {
        return this.ToString(null);
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

    public string ToString(IMetadataSource? metadata) {
        return this.InternalName;
    }

    public override string ToString() {
        return this.ToString(null);
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

    public string ToString(IMetadataSource? metadata) {
        var result = new StringBuilder("closure object(");

        var functionRef = this.Identity.ID.ToFunctionRef([]);

        if (metadata != null) {
            metadata.FormatFunctionRef(functionRef, result);
        } else {
            result.Append(functionRef.ToString(null));
        }

        result.Append($": {this.Identity.Sig.ToString(metadata)})");

        return result.Append(')').ToString();
    }

    public override string ToString() {
        return this.ToString(null);
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
                var name = reader.ReadString()
                    ?? throw new MessagePackSerializationException("missing name value for internal struct identity");

                return new InternalStructIdentity(name);
            }
            
            case "ClosureObject": {
                var sig = MessagePackSerializer.Deserialize<ClosureIdentity>(ref reader, options);
                return new ClosureStructIdentity(sig);
            }
            
            default: {
                throw new MessagePackSerializationException($"illegal struct identity discriminator: {key}");
            }
        }
    }
}
