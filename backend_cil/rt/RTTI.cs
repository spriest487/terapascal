namespace Terapascal.Runtime;

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
