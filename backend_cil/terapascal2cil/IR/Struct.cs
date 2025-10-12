using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

[MessagePackObject]
public record StructDef {
    [Key("identity")]
    public required IStructIdentity Identity { get; init; }
    
    [Key("fields")]
    public required OrderedDictionary<FieldID, StructFieldDef> Fields { get; init; }
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

public interface IStructIdentity;
public record RecordStructIdentity(NamePath Name) : IStructIdentity;
public record ClassStructIdentity(NamePath Name) : IStructIdentity;
public record ArrayStructIdentity(IType ElementType, ulong Size) : IStructIdentity;
public record DynArrayStructIdentity(IType Type) : IStructIdentity;
public record ClosureStructIdentity(ClosureIdentity Identity) : IStructIdentity;

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
