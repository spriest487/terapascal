using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public interface IGlobalRef {
    IGlobalRef ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this;
    }
}

public record FunctionGlobalRef(FunctionRef Ref) : IGlobalRef {
    public IGlobalRef ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new FunctionGlobalRef(new FunctionRef {
            DefID = this.Ref.DefID,
            TypeArgs = this.Ref.TypeArgs?
                .Select(t => t.ResolveGeneric(typeMap))
                .ToArray(),
        });
    }
}

public record StringLiteralGlobalRef(StringID ID) : IGlobalRef;

public record StaticTypeInfoGlobalRef(IType Type) : IGlobalRef {
    public IGlobalRef ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new StaticTypeInfoGlobalRef(this.Type.ResolveGeneric(typeMap));
    }
}

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
                var funcRef = MessagePackSerializer.Deserialize<FunctionRef>(ref reader, options);
                return new FunctionGlobalRef(funcRef);
            }
            case "StringLiteral": {
                var id = reader.ReadUInt64();
                return new StringLiteralGlobalRef(new StringID(id));
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
