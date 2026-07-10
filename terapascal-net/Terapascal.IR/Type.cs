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

    public TypeRef ToTypeRef(IReadOnlyList<IType>? typeArgs) {
        return new TypeRef {
            DefID = this,
            Args = typeArgs,
        };
    }

    public IObjectID ToObjectID(IReadOnlyList<IType> typeArgs) {
        return this.ToTypeRef(typeArgs).ToClassObjectID();
    }

    public IType ToObjectType(IReadOnlyList<IType> typeArgs) {
        return this.ToObjectID(typeArgs).ToObjectType();
    }

    public IType ToWeakObjectType(IReadOnlyList<IType> typeArgs) {
        return this.ToObjectID(typeArgs).ToWeakObjectType();
    }

    public IType ToStructType(IReadOnlyList<IType> typeArgs) {
        return this.ToTypeRef(typeArgs).ToStructType();
    }

    public override string ToString() {
        return this.ID.ToString();
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

    public IType ToInterfacePointerType(IReadOnlyList<IType> typeArgs) {
        return new ObjectType(new InterfaceObjectID(new InterfaceRef {
            DefID = this,
            Args = typeArgs,
        }));
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

public interface IType : IEquatable<IType> {
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

    bool ContainsGenericParams { get; }

    bool IsObjectType() => this switch {
        ObjectType => true,
        WeakObjectType => true,
        _ => false,
    };

    bool IsComplex() => this switch {
        StructType => true,
        VariantType => true,
        ArrayType => true,
        _ => false,
    };

    bool IsInteger() => this switch {
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

    IType? GetDerefType() {
        return this switch {
            PointerType(var inner) => inner,
            TempRefType(var inner) => inner,
            _ => null,
        };
    }

    IType? GetElementType() {
        return this switch {
            PointerType(var inner) => inner,
            ArrayType { Element: var elementType } => elementType,
            ObjectType(ArrayObjectID(var elementType)) => elementType,
            ObjectType(BoxObjectID(var valueType)) => valueType,
            _ => null,
        };
    }

    IType MakeDynArray() {
        return new ObjectType(new ArrayObjectID(this));
    }

    IType MakeBox() {
        return new ObjectType(new BoxObjectID(this));
    }

    IType MakePointer() {
        return new PointerType(this);
    }

    IType MakeTempRef() {
        return new TempRefType(this);
    }

    int? IntrinsicSize() => this switch {
        BoolType or U8Type or I8Type => 1,
        I16Type or U16Type => 2,
        F32Type or U32Type or I32Type => 4,
        F64Type or U64Type or I64Type => 8,
        _ => null,
    };

    TypeRef? GetTypeRef() => this switch {
        ObjectType(ClassObjectID(var id)) => id,
        WeakObjectType(ClassObjectID(var id)) => id,
        StructType(var id) => id,
        VariantType(var id) => id,
        _ => null,
    };

    ITagLocation? GetTagsLocation() => this switch {
        VariantType(var id) => new TypeDefTagLocation(id.DefID),
        StructType(var id) => new TypeDefTagLocation(id.DefID),
        ObjectType(ClassObjectID(var id)) => new TypeDefTagLocation(id.DefID),
        ObjectType(InterfaceObjectID(var id)) => new InterfaceTagLocation(id.DefID),
        _ => null,
    };

    string ToString(IMetadataSource? metadata);

    IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap);

    bool IEquatable<IType>.Equals(IType? other) {
        return Equals(this, other);
    }
}

public abstract record PrimitiveType : IType {
    public bool ContainsGenericParams => false;

    public string ToString(IMetadataSource? metadata) {
        return this.ToString();
    }

    public IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this;
    }
}

public sealed record GenericType(string Name) : IType {
    public bool ContainsGenericParams => true;

    public string ToString(IMetadataSource? metadata) {
        return this.Name;
    }

    public override string ToString() {
        return this.ToString(null);
    }

    public IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        if (!typeMap.TryGetValue(this.Name, out var resolvedType)) {
            throw new InvalidDataException($"missing generic type in type map: {this.Name}");
        }

        return resolvedType;
    }
}

public sealed record NothingType : PrimitiveType {
    public override string ToString() {
        return "nothing";
    }
}

public sealed record PointerType(IType Inner) : IType {
    public bool ContainsGenericParams => this.Inner.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        return $"^{this.Inner.ToString(metadata)}";
    }

    public override string ToString() {
        return this.ToString(null);
    }

    public IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        var inner = this.Inner.ResolveGeneric(typeMap);
        return new PointerType(inner);
    }
}

public sealed record TempRefType(IType Inner) : IType {
    public bool ContainsGenericParams => this.Inner.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        return $"&{this.Inner.ToString(metadata)}";
    }

    public override string ToString() {
        return this.ToString(null);
    }

    public IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        var inner = this.Inner.ResolveGeneric(typeMap);
        return new TempRefType(inner);
    }
}

public sealed record StructType(TypeRef TypeRef) : IType {
    public bool ContainsGenericParams => this.TypeRef.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        if (metadata == null || !metadata.FindStructDef(this.TypeRef.DefID, out var def)) {
            return $"{{struct {this.TypeRef.DefID}}}";
        }

        var path = def.Identity.GetDeclPath();
        if (path == null) {
            return def.Identity.ToString(metadata);
        }

        var typeMap = Util.BuildGenericTypeMap(path.TypeParams ?? [], this.TypeRef.Args ?? []);
        return path.ToGenericName().ResolveGeneric(typeMap).ToString(metadata);
    }

    public override string ToString() {
        return this.ToString(null);
    }

    public IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new StructType(this.TypeRef.ResolveGeneric(typeMap));
    }
}

public sealed record VariantType(TypeRef TypeRef) : IType {
    public bool ContainsGenericParams => this.TypeRef.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        if (metadata == null || !metadata.FindVariantDef(this.TypeRef.DefID, out var def)) {
            return $"{{struct {this.TypeRef.DefID}}}";
        }

        var typeMap = Util.BuildGenericTypeMap(
            def.Name.TypeParams ?? [], 
            this.TypeRef.Args ?? []
        );
            
        return def.Name.ResolveGeneric(typeMap).ToString(metadata);
    }

    public override string ToString() {
        return this.ToString(null);
    }

    public IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new StructType(this.TypeRef.ResolveGeneric(typeMap));
    }
}

public sealed record FunctionType(FunctionSig Sig) : IType {
    public bool ContainsGenericParams => this.Sig.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        return this.Sig.ToString(metadata);
    }

    public override string ToString() {
        return this.ToString(null);
    }

    public IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new FunctionType(this.Sig.ResolveGeneric(typeMap));
    }
}

public sealed record BoolType : PrimitiveType {
    public override string ToString() {
        return "bool";
    }
}

public sealed record U8Type : PrimitiveType {
    public override string ToString() {
        return "u8";
    }
}

public sealed record I8Type : PrimitiveType {
    public override string ToString() {
        return "i8";
    }
}

public sealed record U16Type : PrimitiveType {
    public override string ToString() {
        return "u16";
    }
}

public sealed record I16Type : PrimitiveType {
    public override string ToString() {
        return "i16";
    }
}

public sealed record U32Type : PrimitiveType {
    public override string ToString() {
        return "u32";
    }
}

public sealed record I32Type : PrimitiveType {
    public override string ToString() {
        return "i32";
    }
}

public sealed record U64Type : PrimitiveType {
    public override string ToString() {
        return "u64";
    }
}

public sealed record I64Type : PrimitiveType {
    public override string ToString() {
        return "i64";
    }
}

public sealed record USizeType : PrimitiveType {
    public override string ToString() {
        return "usize";
    }
}

public sealed record ISizeType : PrimitiveType {
    public override string ToString() {
        return "isize";
    }
}

public sealed record F32Type : PrimitiveType {
    public override string ToString() {
        return "f32";
    }
}

public sealed record F64Type : PrimitiveType {
    public override string ToString() {
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

    [IgnoreMember]
    public bool ContainsGenericParams => this.Element.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        return $"array[{this.Length}] of {this.Element.ToString(metadata)}";
    }

    public override string ToString() {
        return this.ToString(null);
    }

    public IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new ArrayType {
            Element = this.Element.ResolveGeneric(typeMap),
            Length = this.Length,
        };
    }
}

public sealed record ObjectType(IObjectID ID) : IType {
    public bool ContainsGenericParams => this.ID.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        return $"*{this.ID.ToString(metadata)}";
    }

    public override string ToString() {
        return this.ToString(null);
    }

    public IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new ObjectType(this.ID.ResolveGeneric(typeMap));
    }
}

public sealed record WeakObjectType(IObjectID ID) : IType {
    public bool ContainsGenericParams => this.ID.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        return $"weak *{this.ID.ToString(metadata)}";
    }

    public override string ToString() {
        return this.ToString(null);
    }

    public IType ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new WeakObjectType(this.ID.ResolveGeneric(typeMap));
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

            case "Generic": {
                var name = reader.ReadString()
                    ?? throw new MessagePackSerializationException("expected name for generic type");;
                return new GenericType(name);
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
                var typeRef = MessagePackSerializer.Deserialize<TypeRef>(ref reader, options);
                return new StructType(typeRef);
            }
            
            case "Variant": {
                var typeRef = MessagePackSerializer.Deserialize<TypeRef>(ref reader, options);
                return new VariantType(typeRef);
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
                var sig = MessagePackSerializer.Deserialize<FunctionSig>(ref reader, options);
                return new FunctionType(sig);
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

public interface IObjectID : IEquatable<IObjectID> {
    bool ContainsGenericParams { get; }

    string ToString(IMetadataSource? metadata);
    
    IObjectID ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap);
    
    IType ToObjectType() {
        return new ObjectType(this);
    }

    IType ToWeakObjectType() {
        return new WeakObjectType(this);
    }

    bool IEquatable<IObjectID>.Equals(IObjectID? other) {
        return Equals(this, other);
    }
}

public sealed record AnyObjectID : IObjectID {
    public bool ContainsGenericParams => false;

    public string ToString(IMetadataSource? metadata) {
        return "any";
    }

    public IObjectID ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this;
    }
}

public sealed record ClassObjectID(TypeRef TypeRef) : IObjectID {
    public bool ContainsGenericParams => this.TypeRef.ContainsGenericParams;

    public static ClassObjectID String => new ClassObjectID(new TypeRef { DefID = TypeDefID.String });
    public static ClassObjectID TypeInfo => new ClassObjectID(new TypeRef { DefID = TypeDefID.TypeInfo });
    public static ClassObjectID MethodInfo => new ClassObjectID(new TypeRef { DefID = TypeDefID.MethodInfo });
    public static ClassObjectID FunctionInfo => new ClassObjectID(new TypeRef { DefID = TypeDefID.FunctionInfo });
    
    public string ToString(IMetadataSource? metadata) {
        if (metadata == null || !metadata.FindStructDef(this.TypeRef.DefID, out var def)) {
            return $"{{class {this.TypeRef.DefID}}}";
        }

        var path = def.Identity.GetDeclPath();
        if (path == null) {
            return def.Identity.ToString(metadata);
        }

        var typeMap = Util.BuildGenericTypeMap(path.TypeParams ?? [], this.TypeRef.Args ?? []);
        return path.ToGenericName().ResolveGeneric(typeMap).ToString(metadata);
    }

    public IObjectID ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new ClassObjectID(this.TypeRef.ResolveGeneric(typeMap));
    }
}

public sealed record InterfaceObjectID(InterfaceRef InterfaceRef) : IObjectID {
    public bool ContainsGenericParams => this.InterfaceRef.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        if (metadata == null || !metadata.FindInterfaceDecl(this.InterfaceRef.DefID, out var ifaceDecl)) {
            return $"{{interface {this.InterfaceRef.DefID}}}";
        }

        var name = ifaceDecl.GetGlobalName();
        var typeMap = Util.BuildGenericTypeMap(name.TypeParams ?? [], this.InterfaceRef.Args ?? []);
        
        return name.ToGenericName().ResolveGeneric(typeMap).ToString(metadata);
    }

    public IObjectID ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new InterfaceObjectID(this.InterfaceRef.ResolveGeneric(typeMap));
    }
}

public sealed record AnyClosureObjectID(FunctionSig Sig) : IObjectID {
    public bool ContainsGenericParams => this.Sig.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        return $"closure of {this.Sig.ToString(metadata)}";
    }

    public IObjectID ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return this;
    }
}

public sealed record ArrayObjectID(IType Element) : IObjectID {
    public bool ContainsGenericParams => this.Element.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        return $"array of {this.Element.ToString(metadata)}";
    }

    public IObjectID ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new ArrayObjectID(this.Element.ResolveGeneric(typeMap));
    }
}

public sealed record BoxObjectID(IType Value) : IObjectID {
    public bool ContainsGenericParams => this.Value.ContainsGenericParams;

    public string ToString(IMetadataSource? metadata) {
        return $"box of {this.Value.ToString(metadata)}";
    }

    public IObjectID ResolveGeneric(IReadOnlyDictionary<string, IType> typeMap) {
        return new BoxObjectID(this.Value.ResolveGeneric(typeMap));
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
                var typeRef =  MessagePackSerializer.Deserialize<TypeRef>(ref reader, options);
                return new ClassObjectID(typeRef);
            }

            case "Interface": {
                var ifaceRef =  MessagePackSerializer.Deserialize<InterfaceRef>(ref reader, options);
                return new InterfaceObjectID(ifaceRef);
            }
            
            case "AnyClosure": {
                var sig = MessagePackSerializer.Deserialize<FunctionSig>(ref reader, options);
                return new AnyClosureObjectID(sig);
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
