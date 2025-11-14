namespace Terapascal.Runtime;

public class Object {
    internal int strongCount;
    internal int weakCount;

    public static T Create<T>(bool immortal) where T : Object, new() {
        return new T {
            strongCount = immortal ? -1 : 1,
            weakCount = 0,
        };
    }

    public virtual void Destroy() {
    }
}

public sealed unsafe class String : Object {
    public byte* chars;
    public int len;

    public override string ToString() {
        return SystemFunctions.ReadString(this);
    }
}

public sealed class TypeInfo {
}

public sealed class MethodInfo {
}

public sealed class FunctionInfo {
}

public class ClosureBase : Object { 
    public IntPtr functionPointer;
}
