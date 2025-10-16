using System.Runtime.InteropServices;
using System.Text;

namespace Terapascal.Runtime;

public static class SystemFunctions {
    private static readonly Random random = new Random();
    
    private static unsafe string ReadString(String s) {
        if (s.len == 0) {
            return "";
        }

        return Marshal.PtrToStringAnsi((IntPtr)s.chars, s.len);
    }

    private static unsafe String CreateString(string s) {
        var byteCount = Encoding.UTF8.GetByteCount(s);
        var bytes = GetMem(byteCount + 1);

        fixed (char* chars = s.AsSpan()) {
            Encoding.UTF8.GetBytes(chars, s.Length, bytes, byteCount);
        }

        bytes[byteCount] = 0;

        return new String {
            chars = bytes,
            len = byteCount,
        };
    }

    public static unsafe byte* GetMem(int count) {
        return (byte*)Marshal.AllocHGlobal(count);
    }

    public static unsafe void FreeMem(byte* mem) {
        Marshal.FreeHGlobal((IntPtr)mem);
    }

    public static void WriteLn(String message) {
        Console.WriteLine(ReadString(message));
    }

    public static void Write(String message) {
        Console.Write(ReadString(message));
    }

    public static String Int8ToString(sbyte i) {
        return CreateString(i.ToString());
    }

    public static String UInt8ToString(byte i) {
        return CreateString(i.ToString());
    }

    public static String Int16ToString(short i) {
        return CreateString(i.ToString());
    }

    public static String UInt16ToString(ushort i) {
        return CreateString(i.ToString());
    }

    public static String Int32ToString(int i) {
        return CreateString(i.ToString());
    }

    public static String UInt32ToString(uint i) {
        return CreateString(i.ToString());
    }

    public static String Int64ToString(long i) {
        return CreateString(i.ToString());
    }

    public static String UInt64ToString(ulong i) {
        return CreateString(i.ToString());
    }

    public static String NativeIntToString(nint i) {
        return CreateString(i.ToString());
    }

    public static String NativeUIntToString(nuint i) {
        return CreateString(i.ToString());
    }

    public static unsafe String PointerToString(void* p) {
        return CreateString(Environment.Is64BitProcess ? $"0x{(ulong)p,16}" : $"0x{(ulong)p,8}");
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

    public static unsafe void InvokeMethod(
        MethodInfo method,
        void* instance,
        void** args,
        int argCount,
        void* resultOut
    ) {
        throw new NotImplementedException();
    }
    
    public static unsafe void InvokeFunction(
        MethodInfo method,
        void** args,
        int argCount,
        void* resultOut
    ) {
        throw new NotImplementedException();
    }

    public static int ArrayLengthInternal(object array) {
        return ((ArrayBase)array).len;
    }
    
    public static unsafe object ArraySetLengthInternal(object array, int len, void* defaultVal) {
        throw new NotImplementedException();
    }
}
