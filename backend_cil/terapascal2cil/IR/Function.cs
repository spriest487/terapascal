using System.Text;
using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public interface IFunction {
    FunctionSig Signature() {
        return this switch {
            ExternalFunction(var funcRef) => funcRef.Signature,
            LocalFunction(var funcDef)=> funcDef.Signature,
            _ => throw new NotSupportedException($"unsupported function: {this}"),
        };
    }
}

public record ExternalFunction(ExternalFunctionRef Ref) : IFunction;
public record LocalFunction(FunctionDef Def) : IFunction;

public class FunctionFormatter : IMessagePackFormatter<IFunction> {
    public void Serialize(ref MessagePackWriter writer, IFunction? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IFunction Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var count = reader.ReadMapHeader();
        if (count != 1) {
            throw new MessagePackSerializationException($"unexpected function element count: {count}");
        }

        var key = reader.ReadString();

        switch (key) {
            case "External": {
                var externRef = MessagePackSerializer.Deserialize<ExternalFunctionRef>(ref reader, options);
                return new ExternalFunction(externRef);
            }

            case "Local": {
                var def = MessagePackSerializer.Deserialize<FunctionDef>(ref reader, options);
                return new LocalFunction(def);
            }

            default: {
                throw new MessagePackSerializationException($"illegal function discriminator: {key}");
            }
        }
    }
}

[MessagePackObject]
public record ExternalFunctionRef {
    [Key("symbol")]
    public required string Symbol {
        get;
        init => field = value ?? throw new ArgumentException(nameof(value));
    }
    
    [Key("src")]
    public required string Source { 
        get;
        init => field = value ?? throw new ArgumentException(nameof(value));
    }

    [Key("sig")]
    public required FunctionSig Signature {
        get;
        init => field = value ?? throw new ArgumentException(nameof(value));
    }
}

[MessagePackObject]
public record FunctionDef {
    [Key("debug_name")]
    public string? DebugName { get; init; }

    [Key("body")]
    public required IReadOnlyList<IInstruction> Body {
        get;
        init => field = value ?? throw new ArgumentException(nameof(value));
    }

    [Key("sig")]
    public required FunctionSig Signature {
        get;
        init => field = value ?? throw new ArgumentException(nameof(value));
    }
}

[MessagePackObject]
public class FunctionSig : IEquatable<FunctionSig> {
    [Key("return_ty")]
    public required IType ReturnType {
        get;
        init => field = value ?? IType.Nothing;
    }
    
    [Key("param_tys")]
    public required IReadOnlyList<IType> ParameterTypes { 
        get;
        init => field = value ?? [];
    }

    public bool Equals(FunctionSig? other) {
        if (other == null) {
            return false;
        }

        return this.ReturnType.Equals(other.ReturnType) && this.ParameterTypes.SequenceEqual(other.ParameterTypes);
    }

    public override bool Equals(object? obj) {
        if (obj is not FunctionSig other) {
            return false;
        }

        return this.Equals(other);
    }

    public override int GetHashCode() {
        var hashCode = 54321;

        foreach (var part in this.ParameterTypes) {
            hashCode ^= part.GetHashCode();
        }

        hashCode ^= this.ReturnType.GetHashCode();
        return hashCode;
    }

    public string ToPrettyString(Metadata metadata) {
        var result = new StringBuilder("function(");

        for (var i = 0; i < this.ParameterTypes.Count; i += 1) {
            if (i > 0) {
                result.Append(", ");
            }

            result.Append(this.ParameterTypes[i].ToPrettyString(metadata));
        }
        
        result.Append("): ");
        result.Append(this.ReturnType.ToPrettyString(metadata));

        return result.ToString();
    }
}

[MessagePackObject]
public record FunctionInfo {
    [Key("runtime_name")]
    public required StringID? RuntimeName { get; init; }
    
    [Key("global_name")]
    public required NamePath? GlobalName { get; init; }

    [Key("invoker")]
    public FunctionID? Invoker { get; init; }
    
    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }
}
