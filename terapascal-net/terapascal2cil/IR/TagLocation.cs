using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public interface ITagLocation {
    string GetUniqueName() => this switch {
        InterfaceMethodTagLocation methodLoc => $"Tags_Interface_{methodLoc.InterfaceID.ID}_Method_{methodLoc.MethodIndex}",
        InterfaceTagLocation(var ifaceID) => $"Tags_Interface_{ifaceID.ID}",
        MethodTagLocation methodLoc => $"Tags_Type_{methodLoc.TypeID.ID}_Method_{methodLoc.MethodIndex}",
        TypeDefTagLocation(var defID) => $"Tags_Type_{defID.ID}",
        FunctionTagLocation(var funcID) => $"Tags_Function_{funcID.ID}",
        _ => throw new NotSupportedException($"unsupported tag location: {this}"),
    };

    ITagLocation? MethodLocation(ulong methodIndex) {
        return null;
    }
}

public record TypeDefTagLocation(TypeDefID ID) : ITagLocation {
    public ITagLocation? MethodLocation(ulong methodIndex) {
        return new MethodTagLocation { 
            TypeID = this.ID, 
            MethodIndex = methodIndex, 
        };
    }
}

public record InterfaceTagLocation(InterfaceID ID) : ITagLocation {
    public ITagLocation? MethodLocation(ulong methodIndex) {
        return new InterfaceMethodTagLocation { 
            InterfaceID = this.ID, 
            MethodIndex = methodIndex, 
        };
    }
}

[MessagePackObject]
public record MethodTagLocation : ITagLocation {
    [Key("type_id")]
    public required TypeDefID TypeID { get; init; }

    [Key("method_index")]
    public required ulong MethodIndex { get; init; }
}

[MessagePackObject]
public record InterfaceMethodTagLocation : ITagLocation {
    [Key("iface_id")]
    public InterfaceID InterfaceID { get; init; }

    [Key("method_index")]
    public ulong MethodIndex { get; init; }
}

public record FunctionTagLocation(FunctionID ID) : ITagLocation;

public class TagLocationComparer : IEqualityComparer<ITagLocation> {
    public bool Equals(ITagLocation? x, ITagLocation? y) {
        switch (x, y) {
            case (null, null): return true;
            case (InterfaceMethodTagLocation x1, InterfaceMethodTagLocation y1): return x1.Equals(y1);
            case (InterfaceTagLocation x1, InterfaceTagLocation y1): return x1.Equals(y1);
            case (MethodTagLocation x1, MethodTagLocation y1): return x1.Equals(y1);
            case (TypeDefTagLocation x1, TypeDefTagLocation y1): return x1.Equals(y1);
            case (FunctionTagLocation x1, FunctionTagLocation y1): return x1.Equals(y1);

            default: return false;
        }
    }

    public int GetHashCode(ITagLocation obj) {
        return obj.GetHashCode();
    }
}

public class TagLocationFormatter : IMessagePackFormatter<ITagLocation> {
    public void Serialize(ref MessagePackWriter writer, ITagLocation? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public ITagLocation Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var count = reader.ReadMapHeader();
        if (count != 1) {
            throw new MessagePackSerializationException($"unexpected tag location element count: {count}");
        }

        var key = reader.ReadString();

        switch (key) {
            case "TypeDef": {
                var id = reader.ReadUInt64();
                return new TypeDefTagLocation(new TypeDefID(id));
            }
            case "Interface": {
                var id = reader.ReadUInt64();
                return new InterfaceTagLocation(new InterfaceID(id));
            }
            case "Method": {
                return MessagePackSerializer.Deserialize<MethodTagLocation>(ref reader, options);
            }
            case "InterfaceMethod": {
                return MessagePackSerializer.Deserialize<InterfaceMethodTagLocation>(ref reader, options);
            }
            case "Function": {
                var id = reader.ReadUInt64();
                return new FunctionTagLocation(new FunctionID(id));
            }
            default: {
                throw new MessagePackSerializationException($"illegal tag location ref discriminator: {key}");
            }
        }
    }
}

