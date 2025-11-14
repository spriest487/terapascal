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
            SystemFunctions.RcRetain(arrayObjects[array], weak);
        }
    }

    public static bool ReleaseArray(Array array, bool weak) {
        lock (globalLock) {
            var dead = SystemFunctions.RcRelease(arrayObjects[array], weak);
            if (dead) {
                arrayObjects.Remove(array);
            }

            return dead;
        }
    }

}
