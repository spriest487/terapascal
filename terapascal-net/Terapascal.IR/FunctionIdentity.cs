using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public interface IFunctionIdentity {
    string ToString(IMetadataSource? metadata);
    
    IReadOnlyList<TypeParam>? TypeParams { get; }

    bool HasTypeParams => this.TypeParams is { Count: > 0 };
}

public record GlobalFunctionIdentity(DeclPath Path) : IFunctionIdentity {
    public string ToString(IMetadataSource? metadata) {
        return this.Path.ToString(metadata);
    }

    public IReadOnlyList<TypeParam>? TypeParams => this.Path.TypeParams;
}

[MessagePackObject]
public record MethodFunctionIdentity : IFunctionIdentity {
    [Key("declaring_type")]
    public required IType DeclaringType { get; init; }

    [Key("id")]
    public MethodID ID { get; init; }

    [Key("name")]
    public required string Name { get; init; }

    [Key("type_params")]
    public IReadOnlyList<TypeParam>? TypeParams { get; init; }

    public string ToString(IMetadataSource? metadata) {
        return $"{this.DeclaringType.ToString(metadata)}.{this.Name}";
    }
}

[MessagePackObject]
public record DestructorFunctionIdentity : IFunctionIdentity {
    [Key("declaring_type")]
    public required IType DeclaringType { get; init; }

    [Key("id")]
    public required MethodID ID { get; init; }

    [Key("name")]
    public required string Name { get; init; }

    public string ToString(IMetadataSource? metadata) {
        return $"{this.DeclaringType.ToString(metadata)}.{this.Name}";
    }

    [IgnoreMember]
    public IReadOnlyList<TypeParam>? TypeParams => null;
}

[MessagePackObject]
public record InternalFunctionIdentity : IFunctionIdentity {
    [Key("name")]
    public required string Name { get; init; }

    [Key("type_params")]
    public IReadOnlyList<TypeParam>? TypeParams { get; init; }

    public string ToString(IMetadataSource? metadata) {
        return this.Name;
    }
}

public class FunctionIdentityFormatter : IMessagePackFormatter<IFunctionIdentity> {
    public void Serialize(ref MessagePackWriter writer, IFunctionIdentity? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IFunctionIdentity Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.NextMessagePackType != MessagePackType.String) {
            var count = reader.ReadMapHeader();
            if (count != 1) {
                throw new MessagePackSerializationException($"unexpected type element count: {count}");
            }
        }

        var key = reader.ReadString();

        switch (key) {
            case "Global": {
                var path = MessagePackSerializer.Deserialize<DeclPath>(ref reader, options);
                return new GlobalFunctionIdentity(path);
            }
            
            case "Method": {
                return MessagePackSerializer.Deserialize<MethodFunctionIdentity>(ref reader, options);
            }
            
            case "Destructor": {
                return MessagePackSerializer.Deserialize<DestructorFunctionIdentity>(ref reader, options);
            }
            
            case "Internal": {
                return MessagePackSerializer.Deserialize<InternalFunctionIdentity>(ref reader, options);
            }
            
            default: {
                throw new MessagePackSerializationException($"unexpected key: {key}");
            }
        }
    }
}
