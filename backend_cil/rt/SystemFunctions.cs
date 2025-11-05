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

    public static unsafe String CreateString(string s) {
        if (string.IsNullOrEmpty(s)) {
            return new String {
                chars = null,
                len = 0,
            };
        }
        
        var byteCount = Encoding.UTF8.GetByteCount(s);
        var bytes = GetMem(byteCount + 1);

        fixed (char* chars = s) {
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
    
    public static String ReadLn() {
        return CreateString(Console.ReadLine() ?? "");
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
        return CreateString(i.ToString());
    }

    public static String UInt8ToStr(byte i) {
        return CreateString(i.ToString());
    }

    public static String Int16ToStr(short i) {
        return CreateString(i.ToString());
    }

    public static String UInt16ToStr(ushort i) {
        return CreateString(i.ToString());
    }

    public static String Int32ToStr(int i) {
        return CreateString(i.ToString());
    }

    public static String UInt32ToStr(uint i) {
        return CreateString(i.ToString());
    }

    public static String Int64ToStr(long i) {
        return CreateString(i.ToString());
    }

    public static String UInt64ToStr(ulong i) {
        return CreateString(i.ToString());
    }

    public static String NativeIntToStr(IntPtr i) {
        return CreateString(i.ToString());
    }

    public static String NativeUIntToStr(UIntPtr i) {
        return CreateString(i.ToString());
    }

    public static String RealToStr(float f) {
        return CreateString(f.ToString(CultureInfo.InvariantCulture));
    }

    public static unsafe String PointerToStr(void* p) {
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

    public static void RcRetain(object? obj, bool weak) {
    }

    public static bool RcRelease(object? obj, bool weak) {
        return false;
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
        throw new NotImplementedException("dynamic arrays");
    }
    
    public static unsafe object ArraySetLengthInternal(object array, int len, void* defaultVal) {
        throw new NotImplementedException("dynamic arrays");
    }
}
