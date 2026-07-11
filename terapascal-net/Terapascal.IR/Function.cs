using System.Diagnostics.CodeAnalysis;
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
    [Key("body")]
    public required InstructionList Body {
        get;
        init => field = value ?? throw new ArgumentException(nameof(value));
    }

    [Key("sig")]
    public required FunctionSig Signature {
        get;
        init => field = value ?? throw new ArgumentException(nameof(value));
    }

    public FunctionDef ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new FunctionDef {
            Signature = this.Signature.ResolveGeneric(typeMap),
            Body = this.Body.ResolveGeneric(typeMap),
        };
    }
}

[MessagePackObject]
public record FunctionRef {
    [Key("def_id")]
    public required FunctionID DefID { get; init; }
    
    [Key("args")]
    public IReadOnlyList<IType>? TypeArgs { get; init; }

    [IgnoreMember]
    [MemberNotNullWhen(true, nameof(TypeArgs))]
    public bool HasTypeArgs => this.TypeArgs is { Count: > 0 };

    public override int GetHashCode() {
        var hashCode = new HashCode();
        hashCode.Add(this.DefID);

        if (this.TypeArgs != null) {
            foreach (var typeArg in this.TypeArgs) {
                hashCode.Add(typeArg);
            }
        }
        
        return hashCode.ToHashCode();
    }

    public virtual bool Equals(FunctionRef? other) {
        return other != null
            && this.DefID == other.DefID
            && (this.TypeArgs ?? []).SequenceEqual(other.TypeArgs ?? []);
    }

    public string ToString(IMetadataSource? metadata) {
        var result = new StringBuilder();

        if (metadata == null) {
            result.Append(this.DefID);
            NamePath.FormatTypeArgsList(this.TypeArgs, metadata, result);

            return result.ToString();
        }

        metadata.FormatFunctionRef(this, result);
        return result.ToString();
    }

    public override string ToString() {
        return this.ToString(null);
    }
}

[MessagePackObject]
public class FunctionSig : IEquatable<FunctionSig> {
    [Key("result_type")]
    public required IType ResultType {
        get;
        init => field = value ?? IType.Nothing;
    }
    
    [Key("param_types")]
    public required IReadOnlyList<IType> ParameterTypes { 
        get;
        init => field = value ?? [];
    }

    [IgnoreMember]
    public bool ContainsGenericParams => this.ResultType.ContainsGenericParams
        || this.ParameterTypes.Any(t => t.ContainsGenericParams);

    public bool Equals(FunctionSig? other) {
        if (other == null) {
            return false;
        }

        return this.ResultType.Equals(other.ResultType)
            && this.ParameterTypes.SequenceEqual(other.ParameterTypes);
    }

    public override bool Equals(object? obj) {
        if (obj is not FunctionSig other) {
            return false;
        }

        return this.Equals(other);
    }

    public override int GetHashCode() {
        var hashCode = new HashCode();

        foreach (var part in this.ParameterTypes) {
            hashCode.Add(part);
        }

        hashCode.Add(this.ResultType);
        return hashCode.ToHashCode();
    }

    public string ToString(IMetadataSource? metadata) {
        var result = new StringBuilder("function(");

        for (var i = 0; i < this.ParameterTypes.Count; i += 1) {
            if (i > 0) {
                result.Append(", ");
            }

            result.Append(this.ParameterTypes[i].ToString(metadata));
        }
        
        result.Append("): ");
        result.Append(this.ResultType.ToString(metadata));

        return result.ToString();
    }

    public FunctionSig ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        var resultType = this.ResultType.ResolveGeneric(typeMap);
        var paramTypes = this.ParameterTypes.Select(ty => ty.ResolveGeneric(typeMap));

        return new FunctionSig {
            ResultType = resultType,
            ParameterTypes = paramTypes.ToArray(),
        };
    }

    public IType ToFunctionType() {
        return new FunctionType(this);
    }
}

[MessagePackObject]
public record FunctionParamInfo {
    [Key("name")]
    public string? Name { get; init; }
    
    [Key("param_type")]
    public required IType Type { get; init; }

    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }
}

[MessagePackObject]
public record FunctionInfo {
    [Key("identity")]
    public required IFunctionIdentity Identity { get; init; }

    [Key("visibility")]
    public required Visibility Visibility { get; init; }

    [Key("params")]
    public required IReadOnlyList<FunctionParamInfo> Params {
        get;
        init => field = value.ToArrayNonNull();
    }

    [Key("result_type")]
    public required IType ResultType { get; init; }
    
    [Key("runtime_name")]
    public required StringID? RuntimeName { get; init; }

    [Key("invoker")]
    public FunctionID? Invoker { get; init; }
    
    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }

    public FunctionSig Signature() {
        return new FunctionSig {
            ResultType = this.ResultType,
            ParameterTypes = this.Params.Select(p => p.Type).ToArray(),
        };
    }
}
