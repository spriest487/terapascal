// ReSharper disable InconsistentNaming

#nullable disable

using System.Runtime.CompilerServices;

namespace Terapascal.Runtime;

public class Object {
    internal static readonly Dictionary<Type, RcMethodTable> rcMethods = [];

    internal int strongCount;
    internal int weakCount;

    [SpecialName]
    protected internal virtual void Destroy() {
    }

    [SpecialName]
    public static T Create<T>(bool immortal) where T : Object, new() {
        return new T {
            strongCount = immortal ? -1 : 1,
            weakCount = 0,
        };
    }

    [SpecialName]
    public static bool Is<A, B>(A any) {
        if (any is not B instance) {
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

public struct WeakObjectRef<T> where T : class {
    public T objectRef;

    static WeakObjectRef() {
        Object.rcMethods.Add(typeof(WeakObjectRef<T>), new RcMethodTable {
            Retain = selfRef => {
                ref var self = ref __refvalue(selfRef, WeakObjectRef<T>);
                SystemFunctions.RcRetain(ref self.objectRef, weak: true);
            },
            Release = selfRef => {
                ref var self = ref __refvalue(selfRef, WeakObjectRef<T>);
                SystemFunctions.RcRelease(ref self.objectRef, weak: true);
            },
            ArrayRelease = erasedArray => {
                var array = (WeakObjectRef<T>[])erasedArray;
                for (var i = 0; i < array.Length; i += 1) {
                    SystemFunctions.RcRelease(ref array[i].objectRef, weak: true);
                }
            },
        });
    }
}
