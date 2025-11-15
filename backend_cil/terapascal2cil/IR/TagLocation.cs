namespace Terapascal.IR;

public interface ITagLocation {
    string GetUniqueName() => this switch {
        InterfaceMethodTagLocation(var ifaceID, var methodID) => $"Tags_Interface_{ifaceID.ID}_Method_{methodID}",
        InterfaceTagLocation(var ifaceID) => $"Tags_Interface_{ifaceID.ID}",
        MethodTagLocation(var typeID, var methodID) => $"Tags_Type_{typeID.ID}_Method_{methodID}",
        TypeDefTagLocation(var defID) => $"Tags_Type_{defID.ID}",
        FunctionTagLocation(var funcID) => $"Tags_Function_{funcID.ID}",
        _ => throw new NotSupportedException($"unsupported tag location: {this}"),
    };
}

public record TypeDefTagLocation(TypeDefID ID) : ITagLocation;
public record InterfaceTagLocation(InterfaceID ID) : ITagLocation;
public record MethodTagLocation(TypeDefID TypeID, ulong MethodIndex) : ITagLocation;
public record InterfaceMethodTagLocation(InterfaceID Interface, ulong MethodIndex) : ITagLocation;
public record FunctionTagLocation(FunctionID ID) : ITagLocation;

public class TagLocationComparer : IEqualityComparer<ITagLocation> {
    public bool Equals(ITagLocation? x, ITagLocation? y) {
        switch (x, y) {
            case (null, null): return true;
            case (InterfaceMethodTagLocation x1, InterfaceMethodTagLocation y1): return x1.Equals(y1);
            case (InterfaceTagLocation x1, InterfaceTagLocation y1): return x1.Equals(y1);
            case (MethodTagLocation x1, MethodTagLocation y1): return x1.Equals(y1);
            case (TypeDefTagLocation x1, TypeDefTagLocation y1): return x1.Equals(y1);
            case (FunctionTagLocation x1, FunctionTagLocation y1): return x1.Equals(y1);
            
            default: return false;
        }
    }

    public int GetHashCode(ITagLocation obj) {
        return obj.GetHashCode();
    }
}
