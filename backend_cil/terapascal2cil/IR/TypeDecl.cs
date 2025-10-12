using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public interface ITypeDecl;
public record ReservedTypeDecl : ITypeDecl;
public record ForwardTypeDecl(NamePath Name) : ITypeDecl;
public record DefTypeDecl(ITypeDef Def) : ITypeDecl;

public class TypeDeclFormatter : IMessagePackFormatter<ITypeDecl> {
    public void Serialize(ref MessagePackWriter writer, ITypeDecl? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public ITypeDecl Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.NextMessagePackType != MessagePackType.String) {
            var count = reader.ReadMapHeader();
            if (count != 1) {
                throw new MessagePackSerializationException($"unexpected typedecl element count: {count}");
            }
        }

        var key = reader.ReadString();

        switch (key) {
            case "Reserved": {
                return new ReservedTypeDecl();
            }

            case "Forward": {
                var namePath = MessagePackSerializer.Deserialize<NamePath>(ref reader, options);
                return new ForwardTypeDecl(namePath);
            }

            case "Def": {
                var def = MessagePackSerializer.Deserialize<ITypeDef>(ref reader, options);
                return new DefTypeDecl(def);
            }
            
            default: {
                throw new MessagePackSerializationException($"illegal typedecl discriminator: {key}");
            }
        }
    }
}

public interface ITypeDef;
public record StructTypeDef(StructDef Def) : ITypeDef;
public record VariantTypeDef(VariantDef Def) : ITypeDef;
public record FunctionTypeDef(FunctionSig Sig) : ITypeDef;

public class TypeDefFormatter : IMessagePackFormatter<ITypeDef> {
    public void Serialize(ref MessagePackWriter writer, ITypeDef? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public ITypeDef Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var count = reader.ReadMapHeader();
        if (count != 1) {
            throw new MessagePackSerializationException($"unexpected typedecl element count: {count}");
        }

        var key = reader.ReadString();

        switch (key) {
            case "Struct": {
                var def = MessagePackSerializer.Deserialize<StructDef>(ref reader, options);
                return new StructTypeDef(def);
            }

            case "Variant": {
                var def = MessagePackSerializer.Deserialize<VariantDef>(ref reader, options);
                return new VariantTypeDef(def);
            }

            case "Function": {
                var sig = MessagePackSerializer.Deserialize<FunctionSig>(ref reader, options);
                return new FunctionTypeDef(sig);
            }
            
            default: {
                throw new MessagePackSerializationException($"illegal typedecl discriminator: {key}");
            }
        }
    }
}

[MessagePackObject]
public class SetAliasDef {
    [Key("name")]
    public required NamePath? Name { get; init; }
    
    [Key("flags_struct")]
    public required TypeDefID FlagsStruct { get; init; }
}
