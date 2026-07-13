using System.Diagnostics.CodeAnalysis;

namespace Terapascal.IR;

public class MetadataCollection : IMetadataSource {
    public delegate bool TryFindFunc<T>(Metadata metadata, [NotNullWhen(true)] out T? result);

    private readonly List<Metadata> collection;

    public MetadataCollection() {
        this.collection = [];
    }

    public void Add(Metadata metadata) {
        this.collection.Insert(0, metadata);
    }

    private T? Find<T>(Func<Metadata, T?> f) {
        return this.collection.Select(f).FirstOrDefault(result => result != null);
    }

    private bool TryFind<T>(TryFindFunc<T> f, [NotNullWhen(true)] out T? result) {
        foreach (var metadata in this.collection) {
            if (f(metadata, out result)) {
                return true;
            }
        }

        result = default;
        return false;
    }

    private IEnumerable<T> FindAll<T>(Func<Metadata, IEnumerable<T>> f) {
        return this.collection.SelectMany(f);
    }

    public bool FindVariable(VariableID id, [NotNullWhen(true)] out VariableInfo? variableInfo) {
        return this.TryFind((m, out result) => m.FindVariable(id, out result), out variableInfo);
    }

    public bool FindVariantDef(TypeDefID id, [NotNullWhen(true)] out VariantDef? def) {
        return this.TryFind((m, out result) => m.FindVariantDef(id, out result), out def);
    }

    public bool FindStructDef(TypeDefID id, [NotNullWhen(true)] out StructDef? def) {
        return this.TryFind((m, out result) => m.FindStructDef(id, out result), out def);
    }

    public bool FindStringLiteral(StringID id, [NotNullWhen(true)] out string? literal) {
        return this.TryFind((m, out result) => m.FindStringLiteral(id, out result), out literal);
    }

    public IEnumerable<(TypeDefID ID, ITypeDef TypeDef)> GetTypeDefs() {
        return this.FindAll(m => m.GetTypeDefs());
    }

    public bool FindTypeDef(TypeDefID id, [NotNullWhen(true)] out ITypeDef? def) {
        return this.TryFind((m, out result) => m.FindTypeDef(id, out result), out def);
    }

    public bool FindTypeDecl(TypeDefID id, [NotNullWhen(true)] out ITypeDecl? decl) {
        return this.TryFind((m, out result) => m.FindTypeDecl(id, out result), out decl);
    }

    public bool IsTypeDefined(IType type) {
        return this.collection.Any(m => m.IsTypeDefined(type));
    }

    public IEnumerable<(InterfaceID ID, InterfaceDef InterfaceDef)> GetInterfaceDefs() {
        return this.FindAll(m => m.GetInterfaceDefs());
    }

    public bool FindInterfaceDecl(InterfaceID id, [NotNullWhen(true)] out IInterfaceDecl? interfaceDecl) {
        return this.TryFind((m, out result) => m.FindInterfaceDecl(id, out result), out interfaceDecl);
    }

    public IEnumerable<(ITagLocation, IReadOnlyList<TagInfo>)> GetAllTags() {
        return this.FindAll(m => m.GetAllTags());
    }

    public bool TryGetInterfaceDef(InterfaceID id, [NotNullWhen(true)] out InterfaceDef? def) {
        return this.TryFind((m, out result) => m.TryGetInterfaceDef(id, out result), out def);
    }

    public bool TryGetInterfaceImpl(FunctionID functionID, out InterfaceMethodImplRef result) {
        return this.TryFind((m, out result) => m.TryGetInterfaceImpl(functionID, out result), out result);
    }

    public bool GetInterfaceImpls(IType type, [NotNullWhen(true)] out IReadOnlyDictionary<InterfaceRef, InterfaceImpl>? result) {
        return this.TryFind((m, out result) => m.GetInterfaceImpls(type, out result), out result);
    }

    public bool FindFunction(FunctionID id, [NotNullWhen(true)] out FunctionInfo? functionInfo) {
        return this.TryFind((m, out result) => m.FindFunction(id, out result), out functionInfo);
    }

    public bool FindDestructor(IType type, out FunctionID functionID) {
        return this.TryFind((m, out result) => m.FindDestructor(type, out result), out functionID);
    }

    public bool FindClosureSig(TypeDefID id, [NotNullWhen(true)] out FunctionSig? sig) {
        return this.TryFind((m, out result) => m.FindClosureSig(id, out result), out sig);
    }
}
