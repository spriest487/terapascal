using System.Diagnostics;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using FieldAttributes = Mono.Cecil.FieldAttributes;
using MethodAttributes = Mono.Cecil.MethodAttributes;
using TypeAttributes = Mono.Cecil.TypeAttributes;
using static Terapascal.IR.TypeExt;

namespace Terapascal.CIL;

internal class StructFieldRefs {
    internal required Dictionary<IR.FieldID, FieldReference> FieldRefs { get; init; }
}

internal class VariantFieldRefs {
    internal required FieldReference DiscriminatorFieldRef { get; init; }
    internal required List<FieldReference?> CaseFieldRefs { get; init; }
}

public class TypeBuilder {
    public const string VariantDiscriminatorName = "Discriminator";

    public const int PointerSize = 8;
    
    private readonly record struct ArraySig(IR.IType Element, ulong Size);

    private readonly AssemblyBuilder assemblyBuilder;

    private readonly Dictionary<IR.IType, TypeReference> cache;

    private readonly Dictionary<ArraySig, TypeReference> staticArrayTypes;

    private readonly Dictionary<IR.TypeDefID, FunctionPointerType> funcPointerTypes;

    private readonly Dictionary<IR.TypeDefID, StructFieldRefs> structFields;
    private readonly Dictionary<IR.TypeDefID, VariantFieldRefs> variantFields;

    private readonly TypeReference valueType;
    private readonly TypeReference exceptionType;
    private readonly TypeReference clrStringType;

    public TypeReference ExceptionType => this.exceptionType;
    public TypeReference CLRStringType => this.clrStringType;

    public TypeBuilder(AssemblyBuilder assemblyBuilder) {
        this.assemblyBuilder = assemblyBuilder;

        this.cache = new Dictionary<IR.IType, TypeReference>();
        this.staticArrayTypes = new Dictionary<ArraySig, TypeReference>();
        this.funcPointerTypes = new Dictionary<IR.TypeDefID, FunctionPointerType>();

        this.structFields = new Dictionary<IR.TypeDefID, StructFieldRefs>();
        this.variantFields = new Dictionary<IR.TypeDefID, VariantFieldRefs>();

        this.valueType = this.ImportCoreReference("System", "ValueType");
        this.exceptionType = this.ImportCoreReference("System", "Exception");
        this.clrStringType = this.ImportCoreReference("System", "String");

        var rtStringType = this.assemblyBuilder.GetRuntimeTypeRef(nameof(Runtime.String), false);
        var rtTypeInfoType = this.assemblyBuilder.GetRuntimeTypeRef(nameof(Runtime.TypeInfo), false);
        var rtMethodInfoType = this.assemblyBuilder.GetRuntimeTypeRef(nameof(Runtime.MethodInfo), false);
        var rtFunctionInfoType = this.assemblyBuilder.GetRuntimeTypeRef(nameof(Runtime.FunctionInfo), false);
        
        this.cache.Add(IR.IType.String, rtStringType);
        this.cache.Add(IR.IType.TypeInfo, rtTypeInfoType);
        this.cache.Add(IR.IType.MethodInfo, rtMethodInfoType);
        this.cache.Add(IR.IType.FunctionInfo, rtFunctionInfoType);
    }

    public TypeReference BuildTypeRef(IR.IType type) {
        if (this.cache.TryGetValue(type, out var typeRef)) {
            return typeRef;
        }

        var typeSystem = this.assemblyBuilder.TypeSystem;

        typeRef = type switch {
            IR.NothingType => typeSystem.Void,
            IR.BoolType => typeSystem.Boolean,
            IR.U8Type => typeSystem.Byte,
            IR.I8Type => typeSystem.SByte,
            IR.U16Type => typeSystem.UInt16,
            IR.I16Type => typeSystem.Int16,
            IR.U32Type => typeSystem.UInt32,
            IR.I32Type => typeSystem.Int32,
            IR.U64Type => typeSystem.UInt64,
            IR.I64Type => typeSystem.Int64,
            IR.USizeType => typeSystem.UIntPtr,
            IR.ISizeType => typeSystem.IntPtr,
            IR.F32Type => typeSystem.Single,
            IR.F64Type => typeSystem.Double,
            IR.ArrayType { Element: var element, Length: var length } => 
                this.BuildArrayTypeRef(element, length),
            IR.StructType(var id) => this.BuildStructTypeRef(id, isValueType: true),
            IR.VariantType(var id) => this.BuildStructTypeRef(id, isValueType: true),
            IR.FunctionType(var id) => this.BuildFunctionTypeRef(id),
            IR.FlagsType(var id, _) => this.BuildStructTypeRef(id, isValueType: true),
            IR.PointerType(var inner) => this.BuildPointerType(inner),
            IR.RcPointerType(var id) => this.BuildClassTypeRef(id),
            IR.RcWeakPointerType(var id) => this.BuildClassTypeRef(id),
            _ => throw new ArgumentException($"unhandled IR type: {type}"),
        };

        this.cache.Add(type, typeRef);
        return typeRef;
    }

    private TypeReference BuildArrayTypeRef(IR.IType element, ulong length) {
        var arraySig = new ArraySig(element, length);

        if (this.staticArrayTypes.TryGetValue(arraySig, out var arrayTypeRef)) {
            return arrayTypeRef;
        }

        var id = this.staticArrayTypes.Count;
        var typeName = $"StaticArray_Internal{id}";

        var elementTypeRef = this.BuildTypeRef(element);

        var systemTypesNamespace = typeof(Runtime.SystemFunctions).Namespace;

        var typeAttributes = TypeAttributes.NotPublic | TypeAttributes.Sealed;
        var typeDef = new TypeDefinition(systemTypesNamespace, typeName, typeAttributes, this.valueType);
        for (var i = 0UL; i < length; i += 1) {
            var elementField = new FieldDefinition($"Element_{i}", FieldAttributes.Assembly, elementTypeRef);
            typeDef.Fields.Add(elementField);
        }

        this.staticArrayTypes.Add(arraySig, typeDef);
        
        // this.BuildDefaultConstructor(typeDef);

        this.assemblyBuilder.Module.Types.Add(typeDef);

        return typeDef;
    }

    private TypeReference BuildPointerType(IR.IType inner) {
        var innerTypeRef = this.BuildTypeRef(inner);
        if (inner.IsClass()) {
            return innerTypeRef.MakeByReferenceType();
        }

        return innerTypeRef.MakePointerType();
    }

    public static string GetTypeName(IR.TypeDefID id) {
        return string.Intern($"Struct_{id.ID}");
    }

    private TypeReference BuildStructTypeRef(IR.TypeDefID id, bool isValueType) {
        // use native CLR arrays to replace any references to Pascal dynarrays
        foreach (var lib in this.assemblyBuilder.IRLibraries) {
            if (lib.Metadata.GetDynArrayTypeElement(id) is {} elementType) {
                var elementTypeRef = this.BuildTypeRef(elementType);
                return elementTypeRef.MakeArrayType();
            }
        }
        
        var module = this.assemblyBuilder.Module;
        var ns = this.assemblyBuilder.Assembly.Name.Name;

        return new TypeReference(ns, GetTypeName(id), module, module, isValueType);
    }
    
    public static string GetTypeName(IR.InterfaceID id) {
        return string.Intern($"Struct_{id.ID}");
    }

    private TypeReference BuildInterfaceTypeRef(IR.InterfaceID id) {
        var module = this.assemblyBuilder.Module;
        var ns = this.assemblyBuilder.Assembly.Name.Name;
        return new TypeReference(ns, GetTypeName(id), module, module, valueType: false);
    }

    private TypeReference BuildClassTypeRef(IR.IVirtualTypeID id) {
        return id switch {
            IR.AnyVirtualTypeID => this.assemblyBuilder.Module.TypeSystem.Object,
            // in this backend, struct refs are automatically reference types if they're a class
            IR.ClassVirtualTypeID(var classID) => this.BuildStructTypeRef(classID, false),
            IR.InterfaceVirtualTypeID(var interfaceID) => this.BuildInterfaceTypeRef(interfaceID),
            IR.ClosureVirtualTypeID(var closureID) => this.BuildStructTypeRef(closureID, false),
            _ => throw new ArgumentException($"invalid virtual type ID: {id}"),
        };
    }

    public static string GetFunctionTypeName(IR.TypeDefID id) {
        return string.Intern($"Delegate_{id.ID}");
    }
    
    private TypeReference BuildFunctionTypeRef(IR.TypeDefID id) {
        if (!this.funcPointerTypes.TryGetValue(id, out var typeRef)) {
            throw new InvalidOperationException("function pointer types must be populated before any types that reference them");
        }

        return typeRef;
    }

    public static string GetFieldName(IR.FieldID id) {
        return string.Intern($"Field_{id.ID}");
    }

    public void BuildStructDef(IR.TypeDefID id, IR.StructDef structDef) {
        // dynarrays are translated to CLR arrays and don't need a definition
        if (structDef.Identity is IR.DynArrayStructIdentity) {
            return;
        }

        var isClass = structDef.Identity is IR.ClassStructIdentity;
        var typeRef = this.BuildStructTypeRef(id, !isClass);

        var attrs = TypeAttributes.Sealed | TypeAttributes.NotPublic;
        if (isClass) {
            attrs |= TypeAttributes.Class;
        }

        var baseType = isClass
            ? this.assemblyBuilder.Module.TypeSystem.Object
            : this.valueType;

        var typeDef = new TypeDefinition(typeRef.Namespace, typeRef.Name, attrs, baseType);
        
        Debug.Assert(isClass != typeDef.IsValueType);

        var fieldRefs = new Dictionary<IR.FieldID, FieldReference>();

        // TODO: assumes packing works the same as other targets.
        // TODO: we shouldn't calculate a size for class types, they should be unsized
        // array codegen currently requires sized refs so that needs fixing
        var totalSize = isClass ? PointerSize : 0;
        var maxElementSize = PointerSize;
        
        foreach (var (fieldID, structFieldDef) in structDef.Fields) {
            var fieldName = GetFieldName(fieldID);
            var fieldType = this.BuildTypeRef(structFieldDef.Type);

            var fieldAttrs = FieldAttributes.Assembly;

            var fieldDef = new FieldDefinition(fieldName, fieldAttrs, fieldType);
            typeDef.Fields.Add(fieldDef);

            fieldRefs.Add(fieldID, fieldDef);

            if (totalSize != -1 && !isClass) {
                if (!fieldType.IsValueType) {
                    totalSize += PointerSize;
                } else if (fieldType.Resolve() is not { ClassSize: not -1 } sizedTypeDef) {
                    totalSize = -1;
                } else {
                    totalSize += sizedTypeDef.ClassSize;
                    maxElementSize = Math.Max(sizedTypeDef.ClassSize, maxElementSize);
                }
            }
        }

        this.structFields[id] = new StructFieldRefs {
            FieldRefs = fieldRefs,
        };

        if (isClass) {
            this.BuildDefaultConstructor(typeDef);
        }

        // if (!isClass) {
            // typeDef.ClassSize = totalSize;
            // typeDef.PackingSize = (short)Math.Min(maxElementSize, totalSize);
        // }

        this.assemblyBuilder.Module.Types.Add(typeDef);
    }

    public void BuildVariantDef(IR.TypeDefID id, IR.VariantDef def) {
        var typeRef = this.BuildStructTypeRef(id, isValueType: false);

        var typeDef = new TypeDefinition(typeRef.Namespace,
            typeRef.Name,
            TypeAttributes.Sealed | TypeAttributes.NotPublic | TypeAttributes.ExplicitLayout,
            this.valueType
        );

        var discTypeRef = this.BuildTypeRef(new IR.I32Type());
        var discField = new FieldDefinition(VariantDiscriminatorName, FieldAttributes.Assembly, discTypeRef) {
            Offset = 0,
        };

        typeDef.Fields.Add(discField);

        var totalSize = sizeof(int);

        var caseFieldRefs = new List<FieldReference?>(def.Cases.Count);

        var caseTypeRefs = new TypeReference?[def.Cases.Count];
        for (var i = 0; i < def.Cases.Count; i += 1) {
            var dataType = def.Cases[i].Type;
            if (dataType is null or IR.NothingType) {
                continue;
            }
            
            caseTypeRefs[i] = this.BuildTypeRef(dataType);
        }

        var hasObjectMembers = caseTypeRefs.Any(r => r is { IsValueType: false });

        var objectPtrOffset = PointerSize;
        var valueDataOffset = hasObjectMembers ? (objectPtrOffset + PointerSize) : objectPtrOffset;

        var maxDataSize = PointerSize;

        for (var i = 0; i < def.Cases.Count; i++) {
            var dataTypeRef = caseTypeRefs[i];
            if (dataTypeRef is null) {
                caseFieldRefs.Add(null);
                continue;
            }

            var dataFieldName = $"Case_{i}";
            var dataField = new FieldDefinition(dataFieldName, FieldAttributes.Assembly, dataTypeRef) {
                Offset = dataTypeRef.IsValueType ? valueDataOffset : objectPtrOffset,
            };

            typeDef.Fields.Add(dataField);

            if (totalSize != -1) {
                if (!dataTypeRef.IsValueType) {
                    totalSize += PointerSize;
                } else if (dataTypeRef.Resolve() is not { ClassSize: not -1 } sizedTypeDef) {
                    totalSize = -1;
                } else {
                    totalSize += sizedTypeDef.ClassSize;
                    maxDataSize = Math.Max(sizedTypeDef.ClassSize, maxDataSize);
                }
            }

            caseFieldRefs.Add(dataField);
        }

        typeDef.ClassSize = totalSize;
        typeDef.PackingSize = (short)Math.Min(maxDataSize, totalSize);
        
        // this.BuildDefaultConstructor(typeDef);

        this.assemblyBuilder.Module.Types.Add(typeDef);
        
        this.variantFields[id] = new VariantFieldRefs {
            DiscriminatorFieldRef = discField,
            CaseFieldRefs = caseFieldRefs,
        };
    }

    public void BuildFunctionTypeDef(IR.TypeDefID id, IR.FunctionSig sig) {
        var pointerType = new FunctionPointerType {
            ReturnType = this.BuildTypeRef(sig.ReturnType),
        };

        foreach (var paramType in sig.ParameterTypes) {
            var parameterType = this.BuildTypeRef(paramType);
            pointerType.Parameters.Add(new ParameterDefinition(parameterType));
        }
        
        this.funcPointerTypes.Add(id, pointerType);
    }

    public void BuildInterfaceDef(IR.InterfaceID id, IR.InterfaceDef def) {
        // throw new NotImplementedException();
    }

    private void BuildDefaultConstructor(TypeDefinition typeDef) {
        var voidType = this.assemblyBuilder.TypeSystem.Void;
        var methodDef = new MethodDefinition(
            ".ctor",
            MethodAttributes.Assembly | MethodAttributes.HideBySig | MethodAttributes.RTSpecialName |
            MethodAttributes.SpecialName, 
            voidType
        );

        var baseType = this.ResolveCore(typeDef.BaseType);
        var baseCtor = baseType.GetConstructors()
            .Single(ctor => ctor.Parameters.Count == 0);

        var baseCtorRef = this.assemblyBuilder.Module.ImportReference(baseCtor);

        var body = methodDef.Body.GetILProcessor();
        body.Emit(OpCodes.Ldarg_0);
        body.Emit(OpCodes.Call, baseCtorRef);
        body.Emit(OpCodes.Ret);
        
        typeDef.Methods.Add(methodDef);
    }

    public FieldReference GetFieldRef(IR.IType baseType, IR.FieldID fieldID) {
        var structID = baseType switch {
            IR.StructType(var id) => id,
            IR.RcPointerType(IR.ClassVirtualTypeID(var id)) => id,

            _ => throw new ArgumentException($"type {baseType} does not have struct fields"),
        };

        if (!this.structFields.TryGetValue(structID, out var structFieldRefs)
            || !structFieldRefs.FieldRefs.TryGetValue(fieldID, out var fieldRef)) {
            throw new ArgumentException($"struct ID {structID.ID} ({baseType}) does not have field {fieldID.ID}");
        }

        return fieldRef;
    }
    
    public FieldReference GetVariantDiscriminatorFieldRef(IR.IType baseType) {
        var structID = baseType switch {
            IR.VariantType(var id) => id,

            _ => throw new ArgumentException($"type {baseType} is not a variant type"),
        };

        if (!this.variantFields.TryGetValue(structID, out var variantRefs)) {
            throw new ArgumentException($"variant ID {structID.ID} ({baseType}) is not defined yet");
        }

        return variantRefs.DiscriminatorFieldRef;
    }
    
    public FieldReference GetVariantDataFieldRef(IR.IType baseType, ulong tag) {
        var structID = baseType switch {
            IR.VariantType(var id) => id,

            _ => throw new ArgumentException($"type {baseType} is not a variant type"),
        };

        if (!this.variantFields.TryGetValue(structID, out var variantRefs)) {
            throw new ArgumentException($"variant ID {structID.ID} ({baseType}) is not defined yet");
        }

        return variantRefs.CaseFieldRefs[(int)tag]
            ?? throw new ArgumentException($"invalid tag {tag} for variant type {baseType}");
    }

    public TypeReference ImportCoreReference(string ns, string name) {
        var coreLib = this.assemblyBuilder.StandardLibrary;
        var typeRef = new TypeReference(ns, name, coreLib.MainModule, coreLib.Name);
        typeRef = coreLib.MainModule.ImportReference(typeRef);
        typeRef = this.assemblyBuilder.Module.ImportReference(typeRef);

        return typeRef;
    }
    
    public TypeDefinition? ResolveCore(TypeReference reference) {
        var stdlib = this.assemblyBuilder.StandardLibrary;

        foreach (var def in stdlib.MainModule.Types) {
            if (def.FullName == reference.FullName) {
                return def;
            }
        }

        return null;
    }

    public MethodReference ImportCoreReference(MethodReference reference) {
        var coreRef = this.assemblyBuilder.StandardLibrary.MainModule.MetadataResolver.Resolve(reference);

        return this.assemblyBuilder.Module.ImportReference(coreRef);
    }
}
