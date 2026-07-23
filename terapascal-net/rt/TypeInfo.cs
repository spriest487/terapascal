// ReSharper disable InconsistentNaming

#nullable disable
namespace Terapascal.Runtime;

[Flags]
public enum TypeFlags : ulong {
    None = 0,
    Value = 1 << 0,
    Weak = 1 << 1,
    Array = 1 << 2,
    Function = 1 << 3,
}

public sealed class TypeInfo : Object {
    public String name;
    public MethodInfo[] methods;

    public object[] tags;

    public Type impl;

    public ulong flags;
}
