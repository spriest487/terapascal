// ReSharper disable InconsistentNaming

#nullable disable

namespace Terapascal.Runtime;

internal unsafe delegate void RetainDelegate(void* obj, bool weak);
internal unsafe delegate bool ReleaseDelegate(void* obj, bool weak);

public class Object {
    internal static readonly Dictionary<Type, RetainDelegate> retainers = [];
    internal static readonly Dictionary<Type, ReleaseDelegate> releasers = [];

    internal int strongCount;
    internal int weakCount;

    internal static void RegisterRetainer(Type type, RetainDelegate retainer) {
        retainers[type] = retainer;
    }

    internal static void RegisterReleaser(Type type, ReleaseDelegate releaser) {
        releasers[type] = releaser;
    }

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
