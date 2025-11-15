namespace Terapascal.Runtime;

public unsafe delegate void InvokeFunctionAction(void*[] args, void** resultPtr);
public unsafe delegate void InvokeMethodAction(void* selfArg, void*[] args, void** resultPtr);

public static class RTTI {
    private static List<TypeInfo> typeList;
    private static List<FunctionInfo> functionList;

    public static IReadOnlyList<TypeInfo> Types => typeList;
    public static IReadOnlyList<FunctionInfo> Functions => functionList;

    static RTTI() {
        typeList = new List<TypeInfo>();
        functionList = new List<FunctionInfo>();
    }

    public static void AddType(TypeInfo typeInfo) {
        typeList.Add(typeInfo);
    }

    public static void AddFunction(FunctionInfo funcInfo) {
        functionList.Add(funcInfo);
    }
}
