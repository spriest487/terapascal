using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public readonly record struct LocalID(ulong ID) : IComparable<LocalID> {
    public int CompareTo(LocalID other) {
        return this.ID.CompareTo(other.ID);
    }

    public override string ToString() {
        return $"%{this.ID}";
    }
}

public readonly record struct ArgID(ulong ID) : IComparable<ArgID> {
    public int CompareTo(ArgID other) {
        return this.ID.CompareTo(other.ID);
    }

    public override string ToString() {
        return $"%Arg{this.ID}";
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
    public static StringID EmptyString => new StringID(0); 
    
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

public record ResultRef : IRef;

public record ArgRef(ArgID ID) : IRef;
public record LocalRef(LocalID ID) : IRef;
public record GlobalRef(IGlobalRef Global) : IRef;
public record Deref(IValue Value) : IRef;
public record DiscardRef : IRef;

public record FieldRef(FieldRefData Data) : IRef;

[MessagePackObject]
public record FieldRefData {
    [Key("instance")]
    public required IRef Instance {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("instance_type")]
    public required IType InstanceType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("field")]
    public required FieldID FieldID { get; init; }
}

public record ElementRef(ElementRefData Data) : IRef;

[MessagePackObject]
public record ElementRefData {
    [Key("instance")]
    public required IRef Instance {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("instance_type")]
    public required IType InstanceType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("index")]
    public required IValue Index {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
}

public record VariantTagRef(VariantTagRefData Data) : IRef;

[MessagePackObject]
public record VariantTagRefData {
    [Key("instance")]
    public required IRef Instance {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("instance_type")]
    public required IType InstanceType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
}

public record VariantDataRef(VariantDataRefData Data) : IRef;

[MessagePackObject]
public record VariantDataRefData {
    [Key("instance")]
    public required IRef Instance {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("instance_type")]
    public required IType InstanceType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
    
    [Key("case_index")]
    public required ulong CaseIndex { get; init; }
}

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
            case "Result": {
                return new ResultRef();
            }

            case "Arg": {
                var id = new ArgID(reader.ReadUInt64());
                return new ArgRef(id);
            }
            
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
            
            case "Element": {
                var value = MessagePackSerializer.Deserialize<ElementRefData>(ref reader, options);
                return new ElementRef(value);
            }
            
            case "Field": {
                var value = MessagePackSerializer.Deserialize<FieldRefData>(ref reader, options);
                return new FieldRef(value);
            }
            
            case "VariantTag": {
                var value = MessagePackSerializer.Deserialize<VariantTagRefData>(ref reader, options);
                return new VariantTagRef(value);
            }
            
            case "VariantData": {
                var value = MessagePackSerializer.Deserialize<VariantDataRefData>(ref reader, options);
                return new VariantDataRef(value);
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
