namespace Terapascal.Runtime;

public static class ArrayManager {
    private static readonly object globalLock = new object();

    private static readonly Dictionary<Array, Object> arrayObjects = new Dictionary<Array, Object>();

    public static void CreateArray(ref Array instance, int length) {
        var elementType = instance.GetType().GetElementType()!;

        lock (globalLock) {
            if (!arrayObjects.Remove(instance, out var arrayObj)) {
                arrayObj = new Object {
                    strongCount = 1,
                };
            }
            
            instance = Array.CreateInstance(elementType, length);
            arrayObjects.Add(instance, arrayObj);
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
