using System.Text;
using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public interface IValue {
    bool IsLiteral => false;

    string ToString(IMetadataSource? libraryMetadata) {
        var result = new StringBuilder();

        if (libraryMetadata != null) {
            libraryMetadata.FormatValue(this, result);
        } else {
            result.Append(this);
        }

        return result.ToString();
    }

    IValue ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this;
    }
}

public record RefValue(IRef Ref) : IValue {
    public IValue ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new RefValue(this.Ref.ResolveGeneric(typeMap));
    }
}

public record LiteralNilValue : IValue {
    public bool IsLiteral => true;
}

public record LiteralValue<T>(T Value) : IValue {
    public bool IsLiteral => true;
}

public record SizeOfValue(IType Type) : IValue {
    public bool IsLiteral => true;

    public IValue ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new SizeOfValue(this.Type.ResolveGeneric(typeMap));
    }
}

public record DefaultValue(IType Type) : IValue {
    public bool IsLiteral => true;

    public IValue ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new DefaultValue(this.Type.ResolveGeneric(typeMap));
    }
}

public class NullableValueFormatter : IMessagePackFormatter<IValue?> {
    private readonly ValueFormatter valueFormatter = new ValueFormatter();

    public void Serialize(ref MessagePackWriter writer, IValue? value, MessagePackSerializerOptions options) {
        this.valueFormatter.Serialize(ref writer, value, options);
    }

    public IValue? Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.TryReadNil()) {
            return null;
        }
        
        return this.valueFormatter.Deserialize(ref reader, options);
    }
}

public class ValueFormatter : IMessagePackFormatter<IValue> {
    public void Serialize(ref MessagePackWriter writer, IValue? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IValue Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.NextMessagePackType != MessagePackType.String) {
            var count = reader.ReadMapHeader();
            if (count != 1) {
                throw new MessagePackSerializationException($"unexpected value element count: {count}");
            }
        }

        var key = reader.ReadString();

        switch (key) {
            case "Ref": {
                var @ref = MessagePackSerializer.Deserialize<IRef>(ref reader, options);
                return new RefValue(@ref);
            }

            case "LiteralNil": {
                return new LiteralNilValue();
            }

            case "LiteralBool": {
                var val = reader.ReadBoolean();
                return new LiteralValue<bool>(val);
            }
            
            case "LiteralU8": {
                var val = reader.ReadByte();
                return new LiteralValue<byte>(val);
            }

            case "LiteralI8": {
                var val = reader.ReadSByte();
                return new LiteralValue<sbyte>(val);
            }
            
            case "LiteralU16": {
                var val = reader.ReadUInt16();
                return new LiteralValue<ushort>(val);
            }
            case "LiteralI16": {
                var val = reader.ReadInt16();
                return new LiteralValue<short>(val);
            }
            
            case "LiteralU32": {
                var val = reader.ReadUInt32();
                return new LiteralValue<uint>(val);
            }
            case "LiteralI32": {
                var val = reader.ReadInt32();
                return new LiteralValue<int>(val);
            }
            
            case "LiteralU64": {
                var val = reader.ReadUInt64();
                return new LiteralValue<ulong>(val);
            }
            case "LiteralI64": {
                var val = reader.ReadInt64();
                return new LiteralValue<long>(val);
            }
            
            case "LiteralUSize": {
                var val = Environment.Is64BitProcess ? (nuint)reader.ReadUInt64() : reader.ReadUInt32();
                return new LiteralValue<nuint>(val);
            }
            case "LiteralISize": {
                var val = Environment.Is64BitProcess ? (nint)reader.ReadInt64() : reader.ReadInt32();
                return new LiteralValue<nint>(val);
            }
            
            case "LiteralF32": {
                var val = reader.ReadSingle();
                return new LiteralValue<float>(val);
            }
            case "LiteralF64": {
                var val = reader.ReadDouble();
                return new LiteralValue<double>(val);
            }

            case "SizeOf": {
                var type = MessagePackSerializer.Deserialize<IType>(ref reader, options) 
                    ?? throw new MessagePackSerializationException("missing type for SizeOfValue");

                return new SizeOfValue(type);
            }
            
            case "Default": {
                var type = MessagePackSerializer.Deserialize<IType>(ref reader, options) 
                    ?? throw new MessagePackSerializationException("missing type for DefaultValue");

                return new DefaultValue(type);
            }

            default: {
                throw new MessagePackSerializationException($"illegal value discriminator: {key}");
            }
        }
    }
}
