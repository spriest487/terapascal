using MessagePack;
using MessagePack.Formatters;

namespace Terapascal.IR;

public readonly record struct TypeDefID(ulong ID) : IComparable<TypeDefID> {
    public static TypeDefID String => new TypeDefID(1);
    public static TypeDefID TypeInfo => new TypeDefID(2);
    public static TypeDefID MethodInfo => new TypeDefID(3);
    public static TypeDefID FunctionInfo => new TypeDefID(4);

    public int CompareTo(TypeDefID other) {
        return this.ID.CompareTo(other.ID);
    }

    public IType ToObjectType() {
        return new ObjectType(new ClassObjectID(this));
    }
    
    public IType ToWeakObjectType() {
        return new WeakObjectType(new ClassObjectID(this));
    }
    
    public IType ToStructType() {
        return new StructType(this);
    }
}

public class TypeDefIDFormatter : IMessagePackFormatter<TypeDefID> {
    public void Serialize(ref MessagePackWriter writer, TypeDefID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public TypeDefID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var id = reader.ReadUInt64();
        return new TypeDefID(id);
    }
}

public readonly record struct InterfaceID(ulong ID) : IComparable<InterfaceID> {
    public int CompareTo(InterfaceID other) {
        return this.ID.CompareTo(other.ID);
    }

    public IType InterfacePointerType() {
        return new ObjectType(new InterfaceObjectID(this));
    }
}

public class InterfaceIDFormatter : IMessagePackFormatter<InterfaceID> {
    public void Serialize(ref MessagePackWriter writer, InterfaceID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public InterfaceID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var id = reader.ReadUInt64();
        return new InterfaceID(id);
    }
}

public readonly record struct FieldID(ulong ID) : IComparable<FieldID> {
    // magic field used to access the function pointer of any closure instance
    // this is the only field ID it's legal to use with a Field instruction pointing at a closure object type
    public static FieldID ClosurePointerField => new FieldID(0);
    
    public static FieldID TypeInfoName => new FieldID(0);
    public static FieldID TypeInfoMethods => new FieldID(1);
    public static FieldID TypeInfoTags => new FieldID(2);
    public static FieldID TypeInfoImpl => new FieldID(3);
    public static FieldID TypeInfoFlags => new FieldID(4);
    
    public static FieldID MethodInfoName => new FieldID(0);
    public static FieldID MethodInfoOwner => new FieldID(1);
    public static FieldID MethodInfoImpl => new FieldID(2);
    public static FieldID MethodInfoTags => new FieldID(3);
    
    public static FieldID FunctionInfoName => new FieldID(0);
    public static FieldID FunctionInfoImpl => new FieldID(1);
    public static FieldID FunctionInfoTags => new FieldID(2);
    
    public int CompareTo(FieldID other) {
        return this.ID.CompareTo(other.ID);
    }
}

public class FieldIDFormatter : IMessagePackFormatter<FieldID> {
    public void Serialize(ref MessagePackWriter writer, FieldID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public FieldID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var id = reader.ReadUInt64();
        return new FieldID(id);
    }
}

public readonly record struct MethodID(ulong ID) : IComparable<MethodID> {
    public int CompareTo(MethodID other) {
        return this.ID.CompareTo(other.ID);
    }
}

public class MethodIDFormatter : IMessagePackFormatter<MethodID> {
    public void Serialize(ref MessagePackWriter writer, MethodID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public MethodID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        var id = reader.ReadUInt64();
        return new MethodID(id);
    }
}

public interface IType {
    static IType String => new ObjectType(ClassObjectID.String);
    static IType TypeInfo => new ObjectType(ClassObjectID.TypeInfo);
    static IType MethodInfo => new ObjectType(ClassObjectID.MethodInfo);
    static IType FunctionInfo => new ObjectType(ClassObjectID.FunctionInfo);
    
    static IType Any { get; } = new ObjectType(new AnyObjectID());
    static IType Nothing { get; } = new NothingType();

    static IType Bool { get; } = new BoolType();
    static IType U8 { get; } = new U8Type();
    static IType I8 { get; } = new I8Type();
    static IType U16 { get; } = new U16Type();
    static IType I16 { get; } = new I16Type();
    static IType U32 { get; } = new U32Type();
    static IType I32 { get; } = new I32Type();
    static IType U64 { get; } = new U64Type();
    static IType I64 { get; } = new I64Type();
    static IType USize { get; } = new USizeType();
    static IType ISize { get; } = new ISizeType();
    static IType F32 { get; } = new F32Type();
    static IType F64 { get; } = new F64Type();
    
    public bool IsObjectType() => this switch {
        ObjectType => true,
        WeakObjectType => true,
        _ => false,
    };

    public bool IsComplex() => this switch {
        StructType => true,
        VariantType => true,
        ArrayType => true,
        FlagsType => true,
        _ => false,
    };
        
    public bool IsInteger() => this switch {
        F32Type or
            F64Type or
            I16Type or
            I32Type or
            I64Type or
            I8Type or
            ISizeType or
            U16Type or
            U32Type or
            U64Type or
            U8Type or
            USizeType => true,
        _ => false,
    };

    public IType? GetDerefType() {
        return this switch {
            PointerType(var inner) => inner,
            TempRefType(var inner) => inner,
            _ => null,
        };
    }

    public IType MakeDynArray() {
        return new ObjectType(new ArrayObjectID(this));
    }

    public IType MakeBox() {
        return new ObjectType(new BoxObjectID(this));
    }

    public IType MakePointer() {
        return new PointerType(this);
    }

    public int? IntrinsicSize() => this switch {
        BoolType or U8Type or I8Type => 1,
        I16Type or U16Type => 2,
        F32Type or U32Type or I32Type => 4,
        F64Type or U64Type or I64Type => 8,
        _ => null,
    };

    public TypeDefID? GetTypeDefID() => this switch {
        ObjectType(ClassObjectID(var id)) => id,
        WeakObjectType(ClassObjectID(var id)) => id,
        StructType(var id) => id,
        VariantType(var id) => id,
        _ => null,
    };

    public ITagLocation? GetTagsLocation() => this switch {
        VariantType(var id) => new TypeDefTagLocation(id),
        StructType(var id) => new TypeDefTagLocation(id),
        ObjectType(ClassObjectID(var id)) => new TypeDefTagLocation(id),
        ObjectType(InterfaceObjectID(var id)) => new InterfaceTagLocation(id),
        _ => null,
    };

    string ToPrettyString(Metadata metadata);
}

public sealed record NothingType : IType {
    public string ToPrettyString(Metadata metadata) {
        return "nothing";
    }
}

public sealed record PointerType(IType Inner) : IType {
    public string ToPrettyString(Metadata metadata) {
        return $"^{this.Inner.ToPrettyString(metadata)}";
    }
}

public sealed record TempRefType(IType Inner) : IType {
    public string ToPrettyString(Metadata metadata) {
        return $"&{this.Inner.ToPrettyString(metadata)}";
    }
}

public sealed record StructType(TypeDefID ID) : IType {
    public string ToPrettyString(Metadata metadata) {
        if (metadata.FindStructDef(this.ID, out var def)) {
            return def.Identity.ToPrettyString(metadata);
        }
        
        return $"{{struct {this.ID.ID}}}";
    }
}

public sealed record VariantType(TypeDefID ID) : IType {
    public string ToPrettyString(Metadata metadata) {
        if (metadata.FindVariantDef(this.ID, out var def)) {
            return def.Name.ToPrettyString(metadata);
        }
        
        return $"{{struct {this.ID.ID}}}";
    }
}

public sealed record FlagsType(TypeDefID ID) : IType {
    public string ToPrettyString(Metadata metadata) {
        if (metadata.TypeDecls.TryGetValue(this.ID, out var decl)
            && decl is DefTypeDecl(StructTypeDef(var structDef))
            && structDef.Identity is SetFlagsStructIdentity setIdentity) {
            return $"set<{setIdentity.Bits}>";
        }

        return $"{{flags {this.ID.ID}}}";
    }
}

public sealed record FunctionType(TypeDefID ID) : IType {
    public string ToPrettyString(Metadata metadata) {
        if (metadata.TypeDecls.TryGetValue(this.ID, out var decl) 
            && decl is DefTypeDecl(FunctionTypeDef(var sig))) {
            return sig.ToPrettyString(metadata);
        }

        return $"function pointer {this.ID}";
    }
}

public sealed record BoolType : IType {
    public string ToPrettyString(Metadata metadata) {
        return "bool";
    }
}

public sealed record U8Type : IType {
    public string ToPrettyString(Metadata metadata) {
        return "u8";
    }
}

public sealed record I8Type : IType {
    public string ToPrettyString(Metadata metadata) {
        return "i8";
    }
}

public sealed record U16Type : IType {
    public string ToPrettyString(Metadata metadata) {
        return "u16";
    }
}

public sealed record I16Type : IType {
    public string ToPrettyString(Metadata metadata) {
        return "i16";
    }
}

public sealed record U32Type : IType {
    public string ToPrettyString(Metadata metadata) {
        return "u32";
    }
}

public sealed record I32Type : IType {
    public string ToPrettyString(Metadata metadata) {
        return "i32";
    }
}

public sealed record U64Type : IType {
    public string ToPrettyString(Metadata metadata) {
        return "u64";
    }
}

public sealed record I64Type : IType {
    public string ToPrettyString(Metadata metadata) {
        return "i64";
    }
}

public sealed record USizeType : IType {
    public string ToPrettyString(Metadata metadata) {
        return "usize";
    }
}

public sealed record ISizeType : IType {
    public string ToPrettyString(Metadata metadata) {
        return "isize";
    }
}

public sealed record F32Type : IType {
    public string ToPrettyString(Metadata metadata) {
        return "f32";
    }
}

public sealed record F64Type : IType {
    public string ToPrettyString(Metadata metadata) {
        return "f64";
    }
}

[MessagePackObject]
public sealed record ArrayType : IType {
    [Key("element")]
    public required IType Element { 
        get;
        init => field = value ?? throw new ArgumentNullException(nameof(value));
    }
    
    [Key("dim")]
    public required ulong Length { get; init; }

    public string ToPrettyString(Metadata metadata) {
        return $"array[{this.Length}] of {this.Element.ToPrettyString(metadata)}";
    }
}

public sealed record ObjectType(IObjectID ID) : IType {
    public string ToPrettyString(Metadata metadata) {
        return $"*{this.ID.ToPrettyString(metadata)}";
    }
}

public sealed record WeakObjectType(IObjectID ID) : IType {
    public string ToPrettyString(Metadata metadata) {
        return $"weak *{this.ID.ToPrettyString(metadata)}";
    }
}

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
                return IType.Nothing;
            }

            case "Pointer": {
                var inner = MessagePackSerializer.Deserialize<IType>(ref reader, options) 
                    ?? throw new MessagePackSerializationException("expected inner type for pointer");

                return new PointerType(inner);
            }
            
            case "TempRef": {
                var inner = MessagePackSerializer.Deserialize<IType>(ref reader, options) 
                    ?? throw new MessagePackSerializationException("expected inner type for pointer");

                return new TempRefType(inner);
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
                var id = reader.ReadUInt64();
                return new FlagsType(new TypeDefID(id));
            }

            case "Array": {
                return MessagePackSerializer.Deserialize<ArrayType>(ref reader, options);
            }
            
            case "Object": {
                var id = MessagePackSerializer.Deserialize<IObjectID>(ref reader, options);
                return new ObjectType(id);
            }
            
            case "WeakObject": {
                var id = MessagePackSerializer.Deserialize<IObjectID>(ref reader, options);
                return new WeakObjectType(id);
            }
            
            case "Function": {
                var id = reader.ReadUInt64();
                return new FunctionType(new TypeDefID(id));
            }
            
            case "Bool": return IType.Bool;
            case "U8": return IType.U8;
            case "I8": return IType.I8;
            case "U16": return IType.U16;
            case "I16": return IType.I16;
            case "U32": return IType.U32;
            case "I32": return IType.I32;
            case "U64": return IType.U64;
            case "I64": return IType.I64;
            case "USize": return IType.USize;
            case "ISize": return IType.ISize;
            case "F32": return IType.F32;
            case "F64": return IType.F64;

            default: {
                throw new MessagePackSerializationException($"illegal type discriminator: {key}");
            }
        }
    }
}

public interface IObjectID {
    IType ToObjectType() {
        return new ObjectType(this);
    }

    string ToPrettyString(Metadata metadata);
}

public sealed record AnyObjectID : IObjectID {
    public string ToPrettyString(Metadata metadata) {
        return "any";
    }
}

public sealed record ClassObjectID(TypeDefID ID) : IObjectID {
    public static ClassObjectID String => new ClassObjectID(TypeDefID.String);
    public static ClassObjectID TypeInfo => new ClassObjectID(TypeDefID.TypeInfo);
    public static ClassObjectID MethodInfo => new ClassObjectID(TypeDefID.MethodInfo);
    public static ClassObjectID FunctionInfo => new ClassObjectID(TypeDefID.FunctionInfo);
    
    public string ToPrettyString(Metadata metadata) {
        if (metadata.FindStructDef(this.ID, out var def)) {
            return def.Identity.ToPrettyString(metadata);
        }
        
        return $"{{class {this.ID.ID}}}";
    }
}

public sealed record InterfaceObjectID(InterfaceID ID) : IObjectID {
    public string ToPrettyString(Metadata metadata) {
        if (metadata.Interfaces.TryGetValue(this.ID, out var ifaceDecl)) {
            if (ifaceDecl is DefInterfaceDecl(var def)) {
                return def.Name.ToPrettyString(metadata);
            }

            if (ifaceDecl is ForwardInterfaceDecl(var name)) {
                return name.ToPrettyString(metadata);
            }
        }

        return $"{{interface {this.ID.ID}}}";
    }
}

public sealed record ClosureObjectID(TypeDefID FunctionTypeID) : IObjectID {
    public string ToPrettyString(Metadata metadata) {
        if (metadata.TypeDecls.TryGetValue(this.FunctionTypeID, out var typeDecl)
            && typeDecl is DefTypeDecl(FunctionTypeDef(var sig))) {
            return $"closure of {sig.ToPrettyString(metadata)}";
        }

        return $"closure of function {this.FunctionTypeID}";
    }
}

public sealed record ArrayObjectID(IType Element) : IObjectID {
    public string ToPrettyString(Metadata metadata) {
        return $"array of {this.Element.ToPrettyString(metadata)}";
    }
}

public sealed record BoxObjectID(IType Value) : IObjectID {
    public string ToPrettyString(Metadata metadata) {
        return $"box of {this.Value.ToPrettyString(metadata)}";
    }
}

public class ObjectIDFormatter : IMessagePackFormatter<IObjectID> {
    public void Serialize(ref MessagePackWriter writer, IObjectID value, MessagePackSerializerOptions options) {
        throw new NotImplementedException();
    }

    public IObjectID Deserialize(ref MessagePackReader reader, MessagePackSerializerOptions options) {
        if (reader.NextMessagePackType != MessagePackType.String) {
            var count = reader.ReadMapHeader();
            if (count != 1) {
                throw new MessagePackSerializationException($"unexpected object ID element count: {count}");
            }
        }

        var key = reader.ReadString();

        switch (key) {
            case "Any": {
                return new AnyObjectID();
            }

            case "Class": {
                var id = reader.ReadUInt64();
                return new ClassObjectID(new TypeDefID(id));
            }

            case "Interface": {
                var id = reader.ReadUInt64();
                return new InterfaceObjectID(new InterfaceID(id));
            }
            
            case "Closure": {
                var id = reader.ReadUInt64();
                return new ClosureObjectID(new TypeDefID(id));
            }
            
            case "Array": {
                var element = MessagePackSerializer.Deserialize<IType>(ref reader, options);
                return new ArrayObjectID(element);
            }
            
            case "Box": {
                var element = MessagePackSerializer.Deserialize<IType>(ref reader, options);
                return new BoxObjectID(element);
            }

            default: {
                throw new MessagePackSerializationException($"illegal object ID discriminator: {key}");
            }
        }
    }
}
