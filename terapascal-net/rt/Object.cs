// ReSharper disable InconsistentNaming

#nullable disable

namespace Terapascal.Runtime;

public class Object {
    internal static readonly Dictionary<Type, RcMethodTable> rcMethods = [];

    internal int strongCount;
    internal int weakCount;

    protected internal virtual void Destroy() {
    }

    public static T Create<T>(bool immortal) where T : Object, new() {
        return new T {
            strongCount = immortal ? -1 : 1,
            weakCount = 0,
        };
    }

    public static bool Is<T>(object any) {
        if (any is not T instance) {
            return false;
        }

        if (instance is Object obj) {
            return obj.strongCount != 0;
        }

        return true;
    }
}

public unsafe class ClosureBase : Object { 
    public void* functionPointer;
}
