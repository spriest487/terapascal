using System.Runtime.InteropServices;

namespace Terapascal.Runtime;

public class Object {
    internal int strongCount;
    internal int weakCount;

    protected internal virtual void Destroy() {
    }
}

public static class ObjectUtil {
    public static T Create<T>(bool immortal) where T : Object, new() {
        return new T {
            strongCount = immortal ? -1 : 1,
            weakCount = 0,
        };
    }

    public static bool Is<T>(object? any) {
        if (any is not T instance) {
            return false;
        }

        if (instance is Object obj) {
            return obj.strongCount != 0;
        }

        return true;
    }
}

public sealed unsafe class String : Object {
    // TODO
    // move some of the direct string field accesses in the system unit into
    // runtime functions so we can hide these fields in this implementation
    public byte* chars;
    public int len;

    public override string ToString() {
        return SystemFunctions.ReadString(this);
    }

    protected internal override void Destroy() {
        if (this.chars != null) {
            SystemFunctions.FreeMem(this.chars);
            this.chars = null;
        }

        this.len = 0;

        base.Destroy();
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
