using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public readonly record struct LocalID(ulong ID) : IComparable<LocalID> {
    public int CompareTo(LocalID other) {
        return this.ID.CompareTo(other.ID);
    }
}

public readonly record struct FunctionID(ulong ID) : IComparable<FunctionID> {
    public int CompareTo(FunctionID other) {
        return this.ID.CompareTo(other.ID);
    }
}

public readonly record struct VariableID(ulong ID) : IComparable<VariableID> {
    public int CompareTo(VariableID other) {
        return this.ID.CompareTo(other.ID);
    }
}

public readonly record struct StringID(ulong ID) : IComparable<StringID> {
    public int CompareTo(StringID other) {
        return this.ID.CompareTo(other.ID);
    }
}

public readonly record struct StaticClosureID(ulong ID) : IComparable<StaticClosureID> {
    public int CompareTo(StaticClosureID other) {
        return this.ID.CompareTo(other.ID);
    }
}

public class LocalIDFormatter : IMessagePackFormatter<LocalID> {
    public void Serialize(ref MessagePackWriter writer, LocalID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public LocalID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        return new LocalID(reader.ReadUInt64());
    }
}

public class VariableIDFormatter : IMessagePackFormatter<VariableID> {
    public void Serialize(ref MessagePackWriter writer, VariableID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public VariableID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        return new VariableID(reader.ReadUInt64());
    }
}

public class FunctionIDFormatter : IMessagePackFormatter<FunctionID> {
    public void Serialize(ref MessagePackWriter writer, FunctionID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public FunctionID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        return new FunctionID(reader.ReadUInt64());
    }
}

public class StringIDFormatter : IMessagePackFormatter<StringID> {
    public void Serialize(ref MessagePackWriter writer, StringID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public StringID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        return new StringID(reader.ReadUInt64());
    }
}

public class StaticClosureIDFormatter : IMessagePackFormatter<StaticClosureID> {
    public void Serialize(ref MessagePackWriter writer, StaticClosureID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public StaticClosureID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        return new StaticClosureID(reader.ReadUInt64());
    }
}

public interface IRef;
public record LocalRef(LocalID ID) : IRef;
public record GlobalRef(IGlobalRef Global) : IRef;
public record Deref(IValue Value) : IRef;
public record DiscardRef : IRef;

public interface ITagLocation;
public record TypeDefTagLocation(TypeDefID ID) : ITagLocation;
public record InterfaceTagLocation(InterfaceID ID) : ITagLocation;
public record MethodTagLocation(TypeDefID TypeID, ulong MethodIndex) : ITagLocation;
public record InterfaceMethodTagLocation(InterfaceID Interface, ulong MethodIndex) : ITagLocation;
public record FunctionTagLocation(FunctionID ID) : ITagLocation;

public class NullableRefFormatter : IMessagePackFormatter<IRef?> {
    private readonly RefFormatter refFormatter = new RefFormatter();

    public void Serialize(ref MessagePackWriter writer, IRef? value, MessagePackSerializerOptions options) {
        this.refFormatter.Serialize(ref writer, value, options);
    }

    public IRef? Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.TryReadNil()) {
            return null;
        }
        
        return this.refFormatter.Deserialize(ref reader, options);
    }
}

public class RefFormatter : IMessagePackFormatter<IRef> {
    public void Serialize(ref MessagePackWriter writer, IRef? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IRef Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.NextMessagePackType != MessagePackType.String) {
            var count = reader.ReadMapHeader();
            if (count != 1) {
                throw new MessagePackSerializationException($"unexpected ref element count: {count}");
            }
        }

        var key = reader.ReadString();

        switch (key) {
            case "Local": {
                var id = new LocalID(reader.ReadUInt64());
                return new LocalRef(id);
            }

            case "Global": {
                var global = MessagePackSerializer.Deserialize<IGlobalRef>(ref reader, options);
                return new GlobalRef(global);
            }

            case "Deref": {
                var value = MessagePackSerializer.Deserialize<IValue>(ref reader, options);
                return new Deref(value);
            }

            case "Discard": {
                return new DiscardRef();
            }
            
            default: {
                throw new MessagePackSerializationException($"illegal ref discriminator: {key}");
            }
        }
    }
}

public interface IGlobalRef;
public record FunctionGlobalRef(FunctionID ID) : IGlobalRef;
public record StringLiteralGlobalRef(StringID ID) : IGlobalRef;
public record StaticClosureGlobalRef(StaticClosureID ID) : IGlobalRef;
public record StaticTypeInfoGlobalRef(IType Type) : IGlobalRef;
public record StaticFuncInfoGlobalRef(FunctionID ID) : IGlobalRef;
public record VariableGlobalRef(VariableID ID) : IGlobalRef;
public record StaticTagArrayGlobalRef(ITagLocation Location) : IGlobalRef;

public class GlobalRefFormatter : IMessagePackFormatter<IGlobalRef> {
    public void Serialize(ref MessagePackWriter writer, IGlobalRef? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IGlobalRef Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var count = reader.ReadMapHeader();
        if (count != 1) {
            throw new MessagePackSerializationException($"unexpected global ref element count: {count}");
        }

        var key = reader.ReadString();

        switch (key) {
            case "Function": {
                var id = reader.ReadUInt64();
                return new FunctionGlobalRef(new FunctionID(id));
            }
            case "StringLiteral": {
                var id = reader.ReadUInt64();
                return new StringLiteralGlobalRef(new StringID(id));
            }
            case "StaticClosure": {
                var id = reader.ReadUInt64();
                return new StaticClosureGlobalRef(new StaticClosureID(id));
            }
            case "StaticTypeInfo": {
                var type = MessagePackSerializer.Deserialize<IType?>(ref reader, options);
                if (type == null) {
                    throw new MessagePackSerializationException("missing type value for static type info ref");
                }
                return new StaticTypeInfoGlobalRef(type);
            }
            case "StaticFuncInfo": {
                var id = reader.ReadUInt64();
                return new StaticFuncInfoGlobalRef(new FunctionID(id));
            }
            case "Variable": {
                var id = reader.ReadUInt64();
                return new VariableGlobalRef(new VariableID(id));
            }
            case "StaticTagArray": {
                var tagLocation = MessagePackSerializer.Deserialize<ITagLocation>(ref reader, options);
                return new StaticTagArrayGlobalRef(tagLocation);
            }
            default: {
                throw new MessagePackSerializationException($"illegal global ref discriminator: {key}");
            }
        }
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
                var id = reader.ReadUInt64();
                var index = reader.ReadUInt64();
                return new MethodTagLocation(new TypeDefID(id), index);
            }
            case "InterfaceMethod": {
                var id = reader.ReadUInt64();
                var index = reader.ReadUInt64();
                return new InterfaceMethodTagLocation(new InterfaceID(id), index);
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
