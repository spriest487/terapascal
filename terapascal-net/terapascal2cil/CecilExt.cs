using Mono.Cecil;
using Mono.Cecil.Cil;
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

        public MethodReference? FindConstructor(bool isStatic, Type[] paramTypes) {
            return typeDef.GetConstructors()
                .Where(ctor => ctor.IsStatic == isStatic)
                .SingleOrDefault(ctor => ctor.Parameters
                    .Select(p => p.ParameterType)
                    .Zip(paramTypes)
                    .All((pair) => pair.First.Namespace == pair.Second.Namespace
                        && pair.First.Name == pair.Second.Name));
        }

        public MethodDefinition GetOrCreateCCtor() {
            var cctor = typeDef.GetStaticConstructor();
            if (cctor != null) {
                return cctor;
            }

            var attrs = MethodAttributes.Assembly
                | MethodAttributes.Static
                | MethodAttributes.HideBySig
                | MethodAttributes.RTSpecialName
                | MethodAttributes.SpecialName;

            var methodDef = new MethodDefinition(
                ".cctor",
                attrs,
                typeDef.Module.TypeSystem.Void
            );

            typeDef.Methods.Add(methodDef);
            return methodDef;
        }
    }

    extension(ILProcessor ilProcessor) {
        public void EmitLoadLiteral(IR.IValue value) {
            switch (value) {
                case IR.LiteralNilValue: {
                    ilProcessor.Emit(OpCodes.Ldnull);
                    break;
                }
                case IR.LiteralValue<byte>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I4, (int)val);
                    break;
                }
                case IR.LiteralValue<sbyte>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I4, (int)val);
                    break;
                }
                case IR.LiteralValue<ushort>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I4, (int)val);
                    break;
                }
                case IR.LiteralValue<short>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I4, (int)val);
                    break;
                }
                case IR.LiteralValue<uint>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I4, (int)val);
                    break;
                }
                case IR.LiteralValue<int>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I4, (int)val);
                    break;
                }
                case IR.LiteralValue<ulong>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I8, (long)val);
                    break;
                }
                case IR.LiteralValue<long>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I8, (long)val);
                    break;
                }
                case IR.LiteralValue<bool>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I4, val ? 1 : 0);
                    break;
                }
                case IR.LiteralValue<nint>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I8, val);
                    break;
                }
                case IR.LiteralValue<nuint>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_I8, (long)val);
                    break;
                }
                case IR.LiteralValue<float>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_R4, val);
                    break;
                }
                case IR.LiteralValue<double>(var val): {
                    ilProcessor.Emit(OpCodes.Ldc_R8, val);
                    break;
                }

                default: {
                    throw new ArgumentException($"value is not a literal: {value}");
                }
            }
        }
    }
}
