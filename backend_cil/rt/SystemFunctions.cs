using System.Globalization;
using System.Runtime.InteropServices;
using System.Text;

namespace Terapascal.Runtime;

public static class SystemFunctions {
    private static readonly Random random = new Random();

    public static unsafe string ReadString(String s) {
        if (s == null! || s.len == 0 || s.chars == null) {
            return "";
        }

        return Marshal.PtrToStringAnsi((IntPtr)s.chars, s.len);
    }

    public static unsafe String CreateString(string s, bool immortal) {
        var newString = Object.Create<String>(immortal);
        
        if (!string.IsNullOrEmpty(s)) {
            newString.len = Encoding.UTF8.GetByteCount(s);
            newString.chars = GetMem(newString.len + 1);

            fixed (char* chars = s) {
                Encoding.UTF8.GetBytes(chars, s.Length, newString.chars, newString.len);
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
        var messageBytes = new Span<byte>(message.chars, message.len);
        Console.OpenStandardOutput().Write(messageBytes);
        Console.Out.Write(Environment.NewLine);
    }

    public static unsafe void Write(String message) {
        var messageBytes = new Span<byte>(message.chars, message.len);
        Console.OpenStandardOutput().Write(messageBytes);
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

    public static unsafe String PointerToStr(void* p) {
        return CreateString(Environment.Is64BitProcess ? $"0x{(ulong)p,16}" : $"0x{(ulong)p,8}", false);
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

    public static float Nan() {
        return float.NaN;
    }

    public static bool IsNaN(float f) {
        return float.IsNaN(f);
    }

    public static void RcRetain(object? obj, bool weak) {
        switch (obj) {
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

    public static bool RcRelease(object? obj, bool weak) {
        switch (obj) {
            case Object runtimeObj: {
                if (runtimeObj.strongCount < 0) {
                    // immortal
                    return false;
                }

                if (!weak) {
                    if (Interlocked.CompareExchange(ref runtimeObj.strongCount, 0, 1) == 1) {
                        runtimeObj.strongCount = -1;
                        runtimeObj.Destroy();
                        return true;
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

                return false;
            }

            case Array array: {
                return ArrayManager.ReleaseArray(array, weak);
            }

            default: {
                return false;
            }
        }
    }
    
    public static unsafe void InvokeMethod(
        MethodInfo method,
        void* instance,
        void** args,
        int argCount,
        void* resultOut
    ) {
        throw new NotImplementedException("RTTI");
    }
    
    public static unsafe void InvokeFunction(
        MethodInfo method,
        void** args,
        int argCount,
        void* resultOut
    ) {
        throw new NotImplementedException("RTTI");
    }
    
    public static int GetTypeInfoCount() {
        throw new NotImplementedException("RTTI");
    }
    
    public static int GetTypeInfoByIndex(int index) {
        throw new NotImplementedException("RTTI");
    }
    
    public static int FindTypeInfo(string typeName) {
        throw new NotImplementedException("RTTI");
    }
    
    public static TypeInfo GetObjectTypeInfo(object obj) {
        throw new NotImplementedException("RTTI");
    }

    public static FunctionInfo FindFunctionInfo(string name) {
        throw new NotImplementedException("RTTI");
    }
    
    public static int GetFunctionInfoCount() {
        throw new NotImplementedException("RTTI");
    }
    
    public static int GetFunctionInfoByIndex(int index) {
        throw new NotImplementedException("RTTI");
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
