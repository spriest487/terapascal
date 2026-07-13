using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;

namespace Terapascal.Runtime;

using unsafe InvokeFunc = delegate* managed<ref object?, object[]?, ref int, object?>;

public static class SystemFunctions {
    private static readonly Random random = new Random();

    private static readonly DateTime startTime = DateTime.UtcNow;

    public static unsafe string ReadString(String s) {
        if (s == null! || s.len == 0 || s.chars == null) {
            return "";
        }

        return Marshal.PtrToStringAnsi((IntPtr)s.chars, s.len);
    }

    public static unsafe String CreateString(string s, bool immortal) {
        var newString = Object.Create<String>(immortal);

        if (!string.IsNullOrEmpty(s)) {
            var size = Encoding.UTF8.GetMaxByteCount(s.Length);
            newString.chars = GetMem(size + 1);

            fixed (char* chars = s) {
                newString.len = Encoding.UTF8.GetBytes(chars, s.Length, newString.chars, size);
            }

            newString.chars[newString.len] = 0;
        }

        return newString;
    }

    public static unsafe byte* GetMem(int count) {
        return (byte*)Marshal.AllocHGlobal(count);
    }

    public static unsafe void FreeMem(byte* mem) {
        Marshal.FreeHGlobal((IntPtr)mem);
    }

    public static String ReadLn() {
        return CreateString(Console.ReadLine() ?? "", false);
    }

    public static unsafe void WriteLn(String message) {
        var stdout = Console.OpenStandardOutput();
        for (var i = 0; i < message.len; i += 1) {
            stdout.WriteByte(message.chars[i]);
        }
        Console.Out.Write(Environment.NewLine);
        Console.Out.Flush();
    }

    public static unsafe void Write(String message) {
        var stdout = Console.OpenStandardOutput();
        for (var i = 0; i < message.len; i += 1) {
            stdout.WriteByte(message.chars[i]);
        }
    }

    public static String Int8ToStr(sbyte i) {
        return CreateString(i.ToString(), false);
    }

    public static String UInt8ToStr(byte i) {
        return CreateString(i.ToString(), false);
    }

    public static String Int16ToStr(short i) {
        return CreateString(i.ToString(), false);
    }

    public static String UInt16ToStr(ushort i) {
        return CreateString(i.ToString(), false);
    }

    public static String Int32ToStr(int i) {
        return CreateString(i.ToString(), false);
    }

    public static String UInt32ToStr(uint i) {
        return CreateString(i.ToString(), false);
    }

    public static String Int64ToStr(long i) {
        return CreateString(i.ToString(), false);
    }

    public static String UInt64ToStr(ulong i) {
        return CreateString(i.ToString(), false);
    }

    public static String NativeIntToStr(IntPtr i) {
        return CreateString(i.ToString(), false);
    }

    public static String NativeUIntToStr(UIntPtr i) {
        return CreateString(i.ToString(), false);
    }

    public static String RealToStr(float f) {
        return CreateString(f.ToString(CultureInfo.InvariantCulture), false);
    }
    
    public static String Real64ToStr(double f) {
        return CreateString(f.ToString(CultureInfo.InvariantCulture), false);
    }

    public static unsafe String PointerToStr(void* p) {
        return CreateString(Environment.Is64BitProcess ? $"0x{(ulong)p:x16}" : $"0x{(ulong)p:x8}", false);
    }

    public static int StrToInt(String s) {
        return int.Parse(ReadString(s));
    }

    public static int RandomInteger(int rangeStart, int rangeEnd) {
        return random.Next(rangeStart, rangeEnd);
    }

    public static float RandomSingle(float rangeStart, float rangeEnd) {
        return rangeStart + (float)random.NextDouble() * (rangeEnd - rangeStart);
    }

    public static double RandomDouble(double rangeStart, double rangeEnd) {
        return rangeStart + random.NextDouble() * (rangeEnd - rangeStart);
    }

    public static float Pow(float val, float pow) {
        return (float)Math.Pow(val, pow);
    }

    public static float Sqrt(float val) {
        return (float)Math.Sqrt(val);
    }

    public static float Sin(float val) {
        return (float)Math.Sin(val);
    }

    public static float Cos(float val) {
        return (float)Math.Cos(val);
    }

    public static float Tan(float val) {
        return (float)Math.Tan(val);
    }

    public static float ArcSin(float val) {
        return (float)Math.Asin(val);
    }

    public static float ArcCos(float val) {
        return (float)Math.Acos(val);
    }

    public static float ArcTan(float val) {
        return (float)Math.Atan(val);
    }

    public static float Infinity() {
        return float.PositiveInfinity;
    }

    public static bool IsInfinite(float f) {
        return float.IsInfinity(f);
    }

    public static float NaN() {
        return float.NaN;
    }

    public static bool IsNaN(float f) {
        return float.IsNaN(f);
    }
    
    public static double Time() {
        return DateTime.UtcNow.Subtract(startTime).TotalSeconds;
    }

    public static void RcRetain<T>(ref T? obj, bool weak) {
        switch (obj) {
            case null: {
                return;
            }

            case var value when obj.GetType().IsValueType: {
                if (Object.retainers.TryGetValue(obj.GetType(), out var retainer)) {
                    unsafe {
#pragma warning disable CS8500
                        retainer(&value, weak);
#pragma warning restore CS8500
                    }
                }

                break;
            }

            case Object runtimeObj: {
                if (runtimeObj.strongCount < 0) {
                    // immortal
                    return;
                }

                if (weak) {
                    Interlocked.Increment(ref runtimeObj.weakCount);
                } else {
                    var strongCount = Interlocked.Increment(ref runtimeObj.strongCount);
                    if (strongCount == 1) {
                        var err = $"resurrected {runtimeObj.GetType().FullName} with 0 strong refs (+ {runtimeObj.weakCount} weak refs remain)";
                        throw new Error(err);
                    }
                }

                break;
            }

            case Array array: {
                ArrayManager.RetainArray(array, weak);
                break;
            }
        }
    }

    public static void RcRelease<T>(ref T? obj, bool weak) {
        switch (obj) {
            case null: {
                return;
            }

            case var value when obj.GetType().IsValueType: {
                if (Object.releasers.TryGetValue(obj.GetType(), out var releaser)) {
                    unsafe {
#pragma warning disable CS8500
                        if (releaser(&value, weak)) {
#pragma warning restore CS8500
                            obj = default;
                        }
                    }
                }

                break;
            }

            case Object runtimeObj: {
                if (runtimeObj.strongCount < 0) {
                    // immortal
                    break;
                }

                if (!weak) {
                    // make objects temporarily immortal while the destructor runs so the ref count can't
                    // change again for any reason
                    var dead = Interlocked.CompareExchange(ref runtimeObj.strongCount, -1, 1) == 1;

                    if (dead) {
                        runtimeObj.Destroy();
                        runtimeObj.strongCount = 0;
                        obj = default;
                        break;
                    }

                    var strongCount = Interlocked.Decrement(ref runtimeObj.strongCount);
                    if (strongCount < 0) {
                        var err = $"released {runtimeObj.GetType().FullName} with 0 strong refs (+ {runtimeObj.weakCount} weak refs remain)";
                        throw new Error(err);
                    }
                } else {
                    if (Interlocked.Decrement(ref runtimeObj.weakCount) < 0) {
                        var err = $"released {runtimeObj.GetType().FullName} with 0 weak refs (+ {runtimeObj.strongCount} strong refs remain)";
                        throw new Error(err);
                    }
                }

                break;
            }

            case Array array: {
                if (ArrayManager.ReleaseArray(array, weak)) {
                    obj = default;
                }

                break;
            }
        }
    }

    public static unsafe object? InvokeMethod(
        MethodInfo? methodInfo,
        ref object? instance,
        object[]? args,
        ref int errorCode
    ) {
        if (methodInfo?.impl == null || methodInfo.impl.invoker == null) {
            errorCode = 2;
            return null;
        }

        var invoker = (InvokeFunc)methodInfo.impl.invoker;

        return invoker(ref instance, args, ref errorCode);
    }

    public static unsafe object? InvokeFunction(
        FunctionInfo? functionInfo,
        object[]? args,
        ref int errorCode
    ) {
        if (functionInfo?.impl == null || functionInfo.impl.invoker == null) {
            errorCode = 2;
            return null;
        }

        var invoker = (InvokeFunc)functionInfo.impl.invoker;

        object? self = null;
        return invoker(ref self, args, ref errorCode);
    }

    public static int GetTypeInfoCount() {
        return RTTI.Types.Count;
    }

    public static TypeInfo? GetTypeInfoByIndex(int index) {
        if (index < 0 || index >= RTTI.Types.Count) {
            return null;
        }

        return RTTI.Types[index];
    }

    public static TypeInfo? FindTypeInfo(String typeName) {
        for (var i = 0; i < RTTI.Types.Count; i += 1) {
            var typeInfo = RTTI.Types[i];
            if (typeInfo.name.Equals(typeName)) {
                return typeInfo;
            }
        }

        return null;
    }

    public static TypeInfo? GetObjectTypeInfo(object? obj) {
        if (obj is null or Object { strongCount: 0 }) {
            return null;
        }

        var objType = obj.GetType();

        for (var i = 0; i < RTTI.Types.Count; i += 1) {
            var typeInfo = RTTI.Types[i];

            // skip weak types, they have a duplicate ref to the same actual type as the strong pointer type
            if ((typeInfo.flags & (ulong)TypeFlags.Weak) != 0) {
                continue;
            }

            if (typeInfo.impl == objType) {
                return typeInfo;
            }
        }

        return null;
    }

    public static FunctionInfo? FindFunctionInfo(String name) {
        for (var i = 0; i < RTTI.Functions.Count; i += 1) {
            var funcInfo = RTTI.Functions[i];
            if (funcInfo.name.Equals(name)) {
                return funcInfo;
            }
        }

        return null;
    }

    public static int GetFunctionInfoCount() {
        return RTTI.Functions.Count;
    }

    public static FunctionInfo? GetFunctionInfoByIndex(int index) {
        if (index < 0 || index >= RTTI.Functions.Count) {
            return null;
        }

        return RTTI.Functions[index];
    }

    public static int ArrayLengthInternal(object array) {
        var asArray = (Array)array;
        return asArray.Length;
    }

    public static void ArrayCreateInternal(ref object array, int len) {
        var arrayInstance = (Array)array;
        ArrayManager.CreateArray(ref arrayInstance, len);

        array = arrayInstance;
    }
}
