using System.Runtime.CompilerServices;

namespace Terapascal.Runtime;

public static class ArrayManager {
    private static readonly object globalLock = new object();

    private static readonly Dictionary<Array, Object> arrayObjects = new Dictionary<Array, Object>();

    public static T[] CreateArray<T>(int length, bool immortal) {
        var arrayObj = new Object {
            strongCount = immortal ? -1 : 1,
        };

        lock (globalLock) {
            var instance = new T[length];
            arrayObjects.Add(instance, arrayObj);

            return instance;
        }
    }

    // allocate new storage for an array which must previously have been allocated in user code
    // this method is used in the implementation of private functions in the System unit to provide a way
    // to create an uninitialized array of any type without invoking generics or directly using the NewArray
    // instruction.
    // in this C# implementation we use the initial array (assumed to be 0) to determine the element type only,
    // and replace it with a new instance of the correct length. the old instance is assumed to have been allocated
    // in user code e.g. must exist as a managed array or an error will be thrown
    public static void CreateArray(ref Array array, int length) {
        var elementType = array.GetType().GetElementType()!;

        lock (globalLock) {
            // if reallocating an immortal array, the new one is also immortal
            var immortal = arrayObjects[array].strongCount < 0;

            var arrayObj = new Object {
                strongCount = immortal ? -1 : 1,
            };
            
            var newArray = Array.CreateInstance(elementType, length);
            arrayObjects.Add(newArray, arrayObj);

            ReleaseArray(array, false);

            array = newArray;
        }
    }

    public static void RetainArray(Array array, bool weak) {
        lock (globalLock) {
            if (arrayObjects.TryGetValue(array, out var arrayObj)) {
                SystemFunctions.RcRetain(arrayObj, weak);
            }
        }
    }

    public static bool ReleaseArray(Array array, bool weak) {
        lock (globalLock) {
            if (!arrayObjects.TryGetValue(array, out var arrayObj)) {
                return false;
            }

            var dead = SystemFunctions.RcRelease(arrayObj, weak);
            if (dead) {
                arrayObjects.Remove(array);
            }

            return dead;
        }
    }

}
