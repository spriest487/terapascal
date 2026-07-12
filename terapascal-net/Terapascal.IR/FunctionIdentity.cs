using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public interface IFunctionIdentity {
    string ToString(IMetadataSource? metadata);
    
    IReadOnlyList<TypeParam>? TypeParams { get; }

    bool HasTypeParams => this.TypeParams is { Count: > 0 };

    void GetInvocationTypeParams(IMetadataSource metadata, List<TypeParam> result);

    protected static void GetInvocationTypeParams(
        IType? declaringType,
        IReadOnlyList<TypeParam>? typeParams,
        IMetadataSource metadata,
        List<TypeParam> result
    ) {
        if (declaringType != null
            && metadata.FindDeclPath(declaringType, out var declPath)
            && declPath.HasTypeParams
        ) {
            result.AddRange(declPath.TypeParams);
        }

        if (typeParams != null) {
            result.AddRange(typeParams);
        }
    }
}

public record GlobalFunctionIdentity(DeclPath Path) : IFunctionIdentity {
    public string ToString(IMetadataSource? metadata) {
        return this.Path.ToString(metadata);
    }

    public IReadOnlyList<TypeParam>? TypeParams => this.Path.TypeParams;

    public void GetInvocationTypeParams(IMetadataSource metadata, List<TypeParam> result) {
        IFunctionIdentity.GetInvocationTypeParams(null, this.TypeParams, metadata, result);
    }
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

    [Key("is_instance_method")]
    public required bool IsInstanceMethod { get; init; }

    public void GetInvocationTypeParams(IMetadataSource metadata, List<TypeParam> result) {
        IFunctionIdentity.GetInvocationTypeParams(this.DeclaringType, this.TypeParams, metadata, result);
    }

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

    public void GetInvocationTypeParams(IMetadataSource metadata, List<TypeParam> result) {
        IFunctionIdentity.GetInvocationTypeParams(this.DeclaringType, this.TypeParams, metadata, result);
    }
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

    public void GetInvocationTypeParams(IMetadataSource metadata, List<TypeParam> result) {
        IFunctionIdentity.GetInvocationTypeParams(null, this.TypeParams, metadata, result);
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
