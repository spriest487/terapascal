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
    string ToPrettyString(Metadata metadata);
}

public record RecordStructIdentity(NamePath Name) : IStructIdentity {
    public string ToPrettyString(Metadata metadata) {
        return this.Name.ToPrettyString(metadata);
    }
}

public record ClassStructIdentity(NamePath Name) : IStructIdentity {
    public string ToPrettyString(Metadata metadata) {
        return this.Name.ToPrettyString(metadata);
    }
}

public record ArrayStructIdentity(IType ElementType, ulong Size) : IStructIdentity {
    public string ToPrettyString(Metadata metadata) {
        return $"{this.ElementType.ToPrettyString(metadata)}[{this.Size}]";
    }
}

public record DynArrayStructIdentity(IType Type) : IStructIdentity {
    public string ToPrettyString(Metadata metadata) {
        return $"array of {this.Type.ToPrettyString(metadata)}";
    }
}

public record ClosureStructIdentity(ClosureIdentity Identity) : IStructIdentity {
    public string ToPrettyString(Metadata metadata) {
        if (metadata.TypeDecls.TryGetValue(this.Identity.VirtualFunctionType, out var typeDecl)
            && typeDecl is DefTypeDecl(FunctionTypeDef(var sig))) {
            return $"closure of {sig.ToPrettyString(metadata)}";
        }

        return $"closure of function {this.Identity.VirtualFunctionType.ID}";
    }
}

[MessagePackObject]
public readonly record struct ClosureIdentity {
    [Key("virt_func_ty")]
    public TypeDefID VirtualFunctionType { get; init; }
    
    [Key("id")]
    public FunctionID ID { get; init; }
}

[MessagePackObject]
public readonly record struct SetFlagsStructIdentity : IStructIdentity {
    [Key("bits")]
    public required ulong Bits { get; init; }

    public string ToPrettyString(Metadata metadata) {
        return $"set<{this.Bits}>";
    }
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
                var name = MessagePackSerializer.Deserialize<NamePath>(ref reader, options);
                return new RecordStructIdentity(name);
            }

            case "Class": {
                var name = MessagePackSerializer.Deserialize<NamePath>(ref reader, options);
                return new ClassStructIdentity(name);
            }

            case "Array": {
                var (elementType, size) = reader.ReadPair<IType, ulong>(options);
                return new ArrayStructIdentity(elementType, size);
            }
            
            case "DynArray": {
                var elementType = MessagePackSerializer.Deserialize<IType>(ref reader, options);
                return new DynArrayStructIdentity(elementType);
            }
            
            case "Closure": {
                var sig = MessagePackSerializer.Deserialize<ClosureIdentity>(ref reader, options);
                return new ClosureStructIdentity(sig);
            }
            
            case "SetFlags": {
                return MessagePackSerializer.Deserialize<SetFlagsStructIdentity>(ref reader, options);
            }
            
            default: {
                throw new MessagePackSerializationException($"illegal struct identity discriminator: {key}");
            }
        }
    }
}
