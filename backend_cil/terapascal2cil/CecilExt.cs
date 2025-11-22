using Mono.Cecil;
using Mono.Cecil.Rocks;

namespace Terapascal.CIL;

public static class CecilExt {
    extension(TypeDefinition typeDef) {
        public IEnumerable<MethodDefinition> GetAllMethods() {
            var next = typeDef;
            while (next != null) {
                foreach (var method in next.Methods) {
                    yield return method;
                }
            
                next = next.BaseType?.Resolve();
            }
        }
        
        public FieldReference? GetFieldByName(string fieldName) {
            return typeDef.Fields.SingleOrDefault(f => f.Name == fieldName);
        }

        public MethodReference? FindConstructor(params Type[] paramTypes) {
            return typeDef.GetConstructors()
                .SingleOrDefault(ctor => ctor.Parameters
                    .Select(p => p.ParameterType)
                    .Zip(paramTypes)
                    .All((pair) => pair.First.Namespace == pair.Second.Namespace
                        && pair.First.Name == pair.Second.Name));
        }
    }
}
