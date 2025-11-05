namespace Terapascal.Runtime;

public sealed unsafe class String {
    public byte* chars;
    public int len;
}

public sealed class TypeInfo {
}

public sealed class MethodInfo {
}

public sealed class FunctionInfo {
}

public class ClosureBase { 
    public IntPtr functionPointer;
}
