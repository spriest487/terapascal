using System.Diagnostics.CodeAnalysis;
using Mono.Cecil;

namespace Terapascal.CIL;

public readonly record struct TypeID(int ID) : IComparable<TypeID> {
    public int CompareTo(TypeID other) {
        return this.ID.CompareTo(other.ID);
    }


    public override string ToString() {
        return $"Type{this.ID}";
    }
};

internal class TypeCache {
    private readonly SortedDictionary<TypeID, IR.IType> types;
    private readonly Dictionary<IR.IType, TypeID> ids;
    private readonly SortedDictionary<TypeID, TypeReference> typeRefs;

    private int nextID;

    public IEnumerable<IR.IType> AllTypes => this.types.Values;

    public TypeCache() {
        this.types = [];
        this.ids = [];
        this.typeRefs = [];

        this.nextID = 1;
    }

    public (TypeID ID, T TypeRef) RegisterTypeWith<T>(
        IR.IType type,
        Func<TypeID, T> createTypeFunc
    )
    where T : TypeReference {
        if (this.ids.TryGetValue(type, out var id)) {
            return (id, (T)this.typeRefs[id]);
        }

        id = new TypeID(this.nextID);        
        this.nextID += 1;

        var typeRef = createTypeFunc(id);

        this.ids.Add(type, id);
        this.types.Add(id, type);
        this.typeRefs.Add(id, typeRef);

        return (id, typeRef);
    }

    public TypeID RegisterType(IR.IType type, TypeReference typeRef) {
        return this.RegisterTypeWith(type, _ => typeRef).ID;
    }

    public bool TryGetType(
        TypeID id, 
        [NotNullWhen(true)] out IR.IType? type,
        [NotNullWhen(true)] out TypeReference? typeRef
    ) {
        if (!this.types.TryGetValue(id, out type)) {
            typeRef = null;
            return false;
        }
        
        typeRef = this.typeRefs[id];
        return true;
    }

    public bool TryGetType(
        IR.IType type,
        out TypeID id,
        [NotNullWhen(true)] out TypeReference? typeRef
    ) {
        if (!this.ids.TryGetValue(type, out id)) {
            typeRef = null;
            return false;
        }
        
        typeRef = this.typeRefs[id];
        return true;
    }

    public TypeID GetTypeID(IR.IType type) {
        if (!this.ids.TryGetValue(type, out var id)) {
            throw new KeyNotFoundException($"type is not registered in type cache: {type}");
        }

        return id;
    }
}
