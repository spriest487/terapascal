using System.Diagnostics.CodeAnalysis;
using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public interface IInterfaceDecl {
    DeclPath GetGlobalName();
}

public record ForwardInterfaceDecl(DeclPath Name) : IInterfaceDecl {
    public DeclPath GetGlobalName() {
        return this.Name;
    }
}

public record DefInterfaceDecl(InterfaceDef Def) : IInterfaceDecl {
    public DeclPath GetGlobalName() {
        return this.Def.Name;
    }
}

[MessagePackObject]
public record InterfaceDef {
    [Key("name")]
    public required DeclPath Name {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("methods")]
    public required IReadOnlyList<InterfaceMethod> Methods { 
        get;
        init => field = value!.ToArrayNonNull();
    }

    [Key("tags")]
    public required IReadOnlyList<TagInfo> Tags {
        get;
        init => field = value.ToArrayNonNull();
    }

    public bool TryFindMethod(string name, [NotNullWhen(true)] out InterfaceMethod? result) {
        foreach (var method in this.Methods) {
            if (method.Name == name) {
                result = method;
                return true;
            }
        }

        result = null;
        return false;
    }
    
    public bool TryFindMethod(MethodID id, [NotNullWhen(true)] out InterfaceMethod? result) {
        if (id.ID >= (ulong)this.Methods.Count) {
            result = null;
            return false;
        }
        
        result = this.Methods[(int)id.ID];
        return true;
    }
}

[MessagePackObject]
public record InterfaceMethod {
    [Key("name")]
    public required string Name {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }

    [Key("result_type")]
    public required IType ResultType {
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
    
    [Key("params")]
    public required IReadOnlyList<FunctionParamInfo> Parameters { 
        get;
        init => field = value!.ToArrayNonNull();
    }
}

[MessagePackObject]
public record InterfaceImpl {
    [Key("methods")]
    public required IReadOnlyDictionary<MethodID, FunctionID> Methods { 
        get;
        init => field = value!.ToDictionaryNonNull();
    }
}

public class InterfaceDeclFormatter : IMessagePackFormatter<IInterfaceDecl> {
    public void Serialize(ref MessagePackWriter writer, IInterfaceDecl? value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IInterfaceDecl Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var count = reader.ReadMapHeader();
        if (count != 1) {
            throw new MessagePackSerializationException($"unexpected interface decl element count: {count}");
        }

        var key = reader.ReadString();

        switch (key) {
            case "Forward": {
                var name = MessagePackSerializer.Deserialize<DeclPath>(ref reader, options);
                return new ForwardInterfaceDecl(name);
            }

            case "Def": {
                var def = MessagePackSerializer.Deserialize<InterfaceDef>(ref reader, options);
                return new DefInterfaceDecl(def);
            }
            
            default: {
                throw new MessagePackSerializationException($"illegal interface decl discriminator: {key}");
            }
        }
    }
}

public readonly record struct InterfaceMethodImplRef {
    public required InterfaceRef Interface { get; init; }
    public required IType ImplType { get; init; }
    public required string MethodName { get; init; }
}
