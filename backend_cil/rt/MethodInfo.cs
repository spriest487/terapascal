// ReSharper disable InconsistentNaming
#nullable disable

namespace Terapascal.Runtime;

public sealed class MethodInfo : Object {
    public String name;
    public TypeInfo owner;
    
    public FunctionInfoImpl impl;

    public object[] tags;
}
