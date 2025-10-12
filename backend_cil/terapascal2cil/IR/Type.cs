using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public readonly record struct TypeDefID(ulong ID);

public class TypeDefIDFormatter : IMessagePackFormatter<TypeDefID> {
    public void Serialize(ref MessagePackWriter writer, TypeDefID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public TypeDefID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var id = reader.ReadUInt64();
        return new TypeDefID(id);
    }
}

public readonly record struct InterfaceID(ulong ID);

public class InterfaceIDFormatter : IMessagePackFormatter<InterfaceID> {
    public void Serialize(ref MessagePackWriter writer, InterfaceID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public InterfaceID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var id = reader.ReadUInt64();
        return new InterfaceID(id);
    }
}

public readonly record struct SetAliasID(ulong ID);

public class SetAliasIDFormatter : IMessagePackFormatter<SetAliasID> {
    public void Serialize(ref MessagePackWriter writer, SetAliasID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public SetAliasID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var id = reader.ReadUInt64();
        return new SetAliasID(id);
    }
}

public readonly record struct FieldID(ulong ID);

public class FieldIDFormatter : IMessagePackFormatter<FieldID> {
    public void Serialize(ref MessagePackWriter writer, FieldID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public FieldID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var id = reader.ReadUInt64();
        return new FieldID(id);
    }
}

public readonly record struct MethodID(ulong ID);

public class MethodIDFormatter : IMessagePackFormatter<MethodID> {
    public void Serialize(ref MessagePackWriter writer, MethodID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public MethodID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var id = reader.ReadUInt64();
        return new MethodID(id);
    }
}

public interface IType;
public record NothingType : IType;
public record PointerType(IType Inner) : IType;
public record StructType(TypeDefID ID) : IType;
public record VariantType(TypeDefID ID) : IType;
public record FlagsType(TypeDefID ID, SetAliasID AliasID) : IType;

public record FunctionType(FunctionID ID) : IType;

public record BoolType : IType;
public record U8Type : IType;
public record I8Type : IType;
public record U16Type : IType;
public record I16Type : IType;
public record U32Type : IType;
public record I32Type : IType;
public record U64Type : IType;
public record I64Type : IType;
public record USizeType : IType;
public record ISizeType : IType;
public record F32Type : IType;
public record F64Type : IType;

[MessagePackObject]
public record ArrayType : IType {
    [Key("element")]
    public required IType Element { 
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
    
    [Key("dim")]
    public required ulong Length { get; init; }
}

public record RcPointerType(IVirtualTypeID ID) : IType;
public record RcWeakPointerType(IVirtualTypeID ID) : IType;

public class NullableTypeFormatter : IMessagePackFormatter<IType?> {
    private readonly TypeFormatter typeFormatter = new TypeFormatter();

    public void Serialize(ref MessagePackWriter writer, IType? value, MessagePackSerializerOptions options) {
        this.typeFormatter.Serialize(ref writer, value, options);
    }

    public IType? Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.TryReadNil()) {
            return null;
        }

        return this.typeFormatter.Deserialize(ref reader, options);
    }
}

public class TypeFormatter : IMessagePackFormatter<IType> {
    public void Serialize(ref MessagePackWriter writer, IType? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IType Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.NextMessagePackType != MessagePackType.String) {
            var count = reader.ReadMapHeader();
            if (count != 1) {
                throw new MessagePackSerializationException($"unexpected type element count: {count}");
            }
        }

        var key = reader.ReadString();

        switch (key) {
            case "Nothing": {
                return new NothingType();
            }

            case "Pointer": {
                var inner = MessagePackSerializer.Deserialize<IType>(ref reader, options) 
                    ?? throw new MessagePackSerializationException("expected inner type for pointer");

                return new PointerType(inner);
            }

            case "Struct": {
                var id = reader.ReadUInt64();
                return new StructType(new TypeDefID(id));
            }
            
            case "Variant": {
                var id = reader.ReadUInt64();
                return new VariantType(new TypeDefID(id));
            }
            
            case "Flags": {
                var (id, aliasId) = reader.ReadPair<ulong, ulong>(options);
                return new FlagsType(new TypeDefID(id), new SetAliasID(aliasId));
            }

            case "Array": {
                return MessagePackSerializer.Deserialize<ArrayType>(ref reader, options);
            }
            
            case "RcPointer": {
                var id = MessagePackSerializer.Deserialize<IVirtualTypeID>(ref reader, options);
                return new RcPointerType(id);
            }
            
            case "RcWeakPointer": {
                var id = MessagePackSerializer.Deserialize<IVirtualTypeID>(ref reader, options);
                return new RcWeakPointerType(id);
            }
            
            case "Function": {
                var id = reader.ReadUInt64();
                return new FunctionType(new FunctionID(id));
            }
            
            case "Bool": return new BoolType();
            case "U8": return new U8Type();
            case "I8": return new I8Type();
            case "U16": return new U16Type();
            case "I16": return new I16Type();
            case "U32": return new U32Type();
            case "I32": return new I32Type();
            case "U64": return new U64Type();
            case "I64": return new I64Type();
            case "USize": return new USizeType();
            case "ISize": return new ISizeType();
            case "F32": return new F32Type();
            case "F64": return new F64Type();

            default: {
                throw new MessagePackSerializationException($"illegal type discriminator: {key}");
            }
        }
    }
}

public interface IVirtualTypeID;

public record AnyVirtualTypeID : IVirtualTypeID;
public record ClassVirtualTypeID(TypeDefID ID) : IVirtualTypeID;
public record InterfaceVirtualTypeID(InterfaceID ID) : IVirtualTypeID;
public record ClosureVirtualTypeID(TypeDefID ID) : IVirtualTypeID;

public class VirtualTypeIDFormatter : IMessagePackFormatter<IVirtualTypeID> {
    public void Serialize(ref MessagePackWriter writer, IVirtualTypeID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IVirtualTypeID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.NextMessagePackType != MessagePackType.String) {
            var count = reader.ReadMapHeader();
            if (count != 1) {
                throw new MessagePackSerializationException($"unexpected virtual type ID element count: {count}");
            }
        }

        var key = reader.ReadString();

        switch (key) {
            case "Any": {
                return new AnyVirtualTypeID();
            }

            case "Class": {
                var id = reader.ReadUInt64();
                return new ClassVirtualTypeID(new TypeDefID(id));
            }

            case "Interface": {
                var id = reader.ReadUInt64();
                return new InterfaceVirtualTypeID(new InterfaceID(id));
            }
            
            case "Closure": {
                var id = reader.ReadUInt64();
                return new ClosureVirtualTypeID(new TypeDefID(id));
            }

            default: {
                throw new MessagePackSerializationException($"illegal virtual type ID discriminator: {key}");
            }
        }
    }
}
