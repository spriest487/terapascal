// ReSharper disable InconsistentNaming

#nullable disable
namespace Terapascal.Runtime;

public sealed class FunctionInfo : Object {
    public String name;

    public FunctionInfoImpl impl;

    public object[] tags;
}

public sealed unsafe class FunctionInfoImpl {
    public MethodInfo method;
    public void* invoker;
}
