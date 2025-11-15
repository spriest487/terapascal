using Mono.Cecil;

namespace Terapascal.CIL;

public static class CecilExt {
    public static IEnumerable<MethodDefinition> GetAllMethods(this TypeDefinition typeDefinition) {
        var next = typeDefinition;
        while (next != null) {
            foreach (var method in next.Methods) {
                yield return method;
            }
            
            next = next.BaseType?.Resolve();
        }
    }
}
