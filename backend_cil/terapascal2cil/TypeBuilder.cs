using System.Diagnostics;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
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
    private readonly TypeReference closureBaseType;

    private readonly FieldReference closurePointerField;

    public TypeReference ExceptionType => this.exceptionType;
    public TypeReference CLRStringType => this.clrStringType;
    
    public TypeReference ClosureBaseType => this.closureBaseType;

    public TypeBuilder(AssemblyBuilder assemblyBuilder) {
        this.assemblyBuilder = assemblyBuilder;

        this.cache = new Dictionary<IR.IType, TypeReference>();
        this.staticArrayTypes = new Dictionary<ArraySig, TypeReference>();
        this.funcPointerTypes = new Dictionary<IR.TypeDefID, FunctionPointerType>();

        this.structFields = new Dictionary<IR.TypeDefID, StructFieldRefs>();
        this.variantFields = new Dictionary<IR.TypeDefID, VariantFieldRefs>();

        this.valueType = this.ImportCoreReference("System", "ValueType", true);
        this.exceptionType = this.ImportCoreReference("System", "Exception", false);
        this.clrStringType = this.ImportCoreReference("System", "String", false);

        var builtinClasses = (Span<(IR.TypeDefID, string)>)[
            (IR.TypeDefID.String, nameof(Runtime.String)),
            (IR.TypeDefID.TypeInfo, nameof(Runtime.TypeInfo)),
            (IR.TypeDefID.MethodInfo, nameof(Runtime.MethodInfo)), 
            (IR.TypeDefID.FunctionInfo, nameof(Runtime.FunctionInfo)),
        ];

        foreach (var (id, name) in builtinClasses) {
            var typeRef = this.assemblyBuilder.GetRuntimeTypeRef(name, false);
            
            this.cache.Add(id.ToClassType(), typeRef);

            var typeDef = typeRef.Resolve();
            var fieldRefs = new Dictionary<IR.FieldID, FieldReference>();
            
            // assume fields in runtime classes are sequential with no padding or ID gaps
            var fieldID = 0UL;
            foreach (var field in typeDef.Fields) {
                fieldRefs.Add(new IR.FieldID(fieldID), this.assemblyBuilder.Module.ImportReference(field));
                fieldID += 1;
            }

            this.structFields.Add(id, new StructFieldRefs {
                FieldRefs = fieldRefs,
            });
        }

        this.closureBaseType = this.assemblyBuilder.GetRuntimeTypeRef(nameof(Runtime.ClosureBase), false);
        var closureTypeDef = this.closureBaseType.Resolve();

        this.closurePointerField = closureTypeDef.Fields
            .Single(f => f.Name == nameof(Runtime.ClosureBase.functionPointer));
        this.closurePointerField = this.assemblyBuilder.Module.ImportReference(this.closurePointerField);
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
            IR.PointerType(var inner) => this.BuildTypeRef(inner).MakePointerType(),
            IR.TempRefType(var inner) => this.BuildTypeRef(inner).MakeByReferenceType(),
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

        var typeAttributes = TypeAttributes.NotPublic | TypeAttributes.Sealed | TypeAttributes.SequentialLayout | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit;
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

    private static string GetTypeName(IR.InterfaceID id) {
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
            IR.ClosureVirtualTypeID(var closureID) => this.closureBaseType,
            _ => throw new ArgumentException($"invalid virtual type ID: {id}"),
        };
    }
    
    private TypeReference BuildFunctionTypeRef(IR.TypeDefID id) {
        if (!this.funcPointerTypes.TryGetValue(id, out var typeRef)) {
            throw new InvalidOperationException("function pointer types must be populated before any types that reference them");
        }

        return typeRef;
    }

    private static string GetFieldName(IR.FieldID id) {
        return string.Intern($"Field_{id.ID}");
    }

    public void BuildStructDef(IR.TypeDefID id, IR.StructDef structDef) {
        // dynarrays are translated to CLR arrays and don't need a definition
        if (structDef.Identity is IR.DynArrayStructIdentity) {
            return;
        }

        var isClosure = structDef.Identity is IR.ClosureStructIdentity;
        var isClass = structDef.Identity is IR.ClassStructIdentity;
        var isValueType = !isClass && !isClosure;

        var typeRef = this.BuildStructTypeRef(id, isValueType);

        var attrs = TypeAttributes.Sealed | TypeAttributes.NotPublic | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit;
        if (!isValueType) {
            attrs |= TypeAttributes.Class;
        } else {
            attrs |= TypeAttributes.SequentialLayout;
        }

        TypeReference baseType;
        if (isClass) {
            baseType = this.assemblyBuilder.Module.TypeSystem.Object;
        } else if (isClosure) {
            baseType = this.closureBaseType;
        } else {
            baseType = this.valueType;
        }

        var typeDef = new TypeDefinition(typeRef.Namespace, typeRef.Name, attrs, baseType);

        Debug.Assert(isValueType == typeDef.IsValueType);

        var fieldRefs = new Dictionary<IR.FieldID, FieldReference>();

        // TODO: assumes packing works the same as other targets.
        // TODO: we shouldn't calculate a size for class types, they should be unsized
        // array codegen currently requires sized refs so that needs fixing
        var totalSize = isValueType ? 0 : PointerSize;
        var maxElementSize = PointerSize;
        
        foreach (var (fieldID, structFieldDef) in structDef.Fields) {
            var fieldName = GetFieldName(fieldID);

            if (isClosure && fieldID.Equals(IR.FieldID.ClosurePointerField)) {
                // the actual pointer field is declared as an IntPtr in the base class
                Debug.Assert(structFieldDef.Type is IR.FunctionType);
                continue;
            }

            var fieldType = this.BuildTypeRef(structFieldDef.Type);

            var fieldAttrs = FieldAttributes.Assembly;

            var fieldDef = new FieldDefinition(fieldName, fieldAttrs, fieldType);
            typeDef.Fields.Add(fieldDef);

            fieldRefs.Add(fieldID, fieldDef);

            if (totalSize != -1 && isValueType) {
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

        if (!isValueType) {
            this.BuildDefaultConstructor(typeDef);
        }

        this.assemblyBuilder.Module.Types.Add(typeDef);
    }

    private int GetValueLayoutSize(IR.IType type, IR.Library library) {
        switch (type) {
            case IR.ArrayType arrayType: {
                var elementSize = this.GetValueLayoutSize(arrayType.Element, library);
                return elementSize * (int)arrayType.Length;
            }

            case IR.U8Type:
            case IR.I8Type:
            case IR.BoolType: {
                return 1;
            }

            case IR.I16Type:
            case IR.U16Type: {
                return 2;
            }

            case IR.U32Type:
            case IR.I32Type:
            case IR.F32Type: {
                return 4;
            }

            case IR.U64Type:
            case IR.I64Type:
            case IR.USizeType:
            case IR.ISizeType:
            case IR.F64Type: {
                return 8;
            }

            case IR.FlagsType(var defID, _): {
                return this.GetStructLayoutSize(defID, library);
                break;
            }
            case IR.NothingType: {
                return 0;
            }
            
            case IR.RcPointerType:
            case IR.RcWeakPointerType:
            case IR.FunctionType:
            case IR.PointerType:
            case IR.TempRefType: {
                return PointerSize;
            }

            case IR.StructType(var id): {
                return this.GetStructLayoutSize(id, library);
            }

            case IR.VariantType(var id): {
                return this.GetVariantLayoutSize(id, library);
            }

            default: {
                throw new NotImplementedException($"IR type: {type}");
            }
        }
    }

    private int GetStructLayoutSize(IR.TypeDefID id, IR.Library library) {
        if (!library.Metadata.FindStructDef(id, out var structDef)) {
            throw new InvalidDataException($"type def not found in metadata: variant {id.ID}");
        }

        // assume structs are tightly packed + padding is handled by the compiler
        var dataSize = 0;
        foreach (var (_, fieldDef) in structDef.Fields) {
            dataSize += this.GetValueLayoutSize(fieldDef.Type, library);
        }

        return dataSize;
    }

    private int GetVariantLayoutSize(IR.TypeDefID id, IR.Library library) {
        if (!library.Metadata.FindVariantDef(id, out var variantDef)) {
            throw new InvalidDataException($"type def not found in metadata: variant {id.ID}");
        }

        return this.GetVariantLayoutSize(variantDef, library);
    }
    
    private int GetVariantLayoutSize(IR.VariantDef variantDef, IR.Library library) {
        var hasObjectData = false;
        var maxDataSize = 0;
        foreach (var caseDef in variantDef.Cases) {
            if (caseDef.Type == null) {
                continue;
            }

            if (caseDef.Type.IsObjectType()) {
                hasObjectData = true;
            } else {
                maxDataSize += this.GetValueLayoutSize(caseDef.Type, library);
            }
        }

        var totalSize = PointerSize + maxDataSize;
        if (hasObjectData) {
            totalSize += PointerSize;
        }

        return totalSize;
    }

    public void BuildVariantDef(IR.TypeDefID id, IR.VariantDef def, IR.Library library) {
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

        var caseFieldRefs = new List<FieldReference?>(def.Cases.Count);

        var caseTypeRefs = new TypeReference?[def.Cases.Count];
        for (var i = 0; i < def.Cases.Count; i += 1) {
            var dataType = def.Cases[i].Type;
            if (dataType is null or IR.NothingType) {
                continue;
            }
            
            caseTypeRefs[i] = this.BuildTypeRef(dataType);
        }

        // it's safe to overlap object references of different types in a union, as long as we use them correctly,
        // which should be enforced by the source language. if there are any object references cases, store them
        // all at the same offset and offset everything else 
        var hasObjectMembers = caseTypeRefs.Any(r => r is { IsValueType: false });

        var valueDataOffset = hasObjectMembers ? (PointerSize * 2) : PointerSize;

        for (var i = 0; i < def.Cases.Count; i++) {
            var dataTypeRef = caseTypeRefs[i];
            if (dataTypeRef is null) {
                caseFieldRefs.Add(null);
                continue;
            }

            var dataFieldName = $"Case_{i}";
            var dataField = new FieldDefinition(dataFieldName, FieldAttributes.Assembly, dataTypeRef) {
                Offset = dataTypeRef.IsValueType ? valueDataOffset : PointerSize,
            };

            typeDef.Fields.Add(dataField);
            caseFieldRefs.Add(dataField);
        }

        // discriminator (pointer sized) + any value members
        typeDef.ClassSize = this.GetVariantLayoutSize(def, library);
        typeDef.PackingSize = 0;
        
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

    public FunctionPointerType GetFunctionPointerType(IR.TypeDefID id) {
        return this.funcPointerTypes[id];
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

        var baseType = this.ResolveCore(typeDef.BaseType) ?? typeDef.BaseType.Resolve();

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
        // the closure field pointer can be accessed either through a closure object pointer
        // (accessing the pointer of an unknown closure type to call it) or directly as a member of a
        // specific closure class (setting the pointer during construction)
        if (fieldID.Equals(IR.FieldID.ClosurePointerField)) {
            if (baseType is IR.RcPointerType(IR.ClosureVirtualTypeID)
                || (baseType is IR.StructType(var closureStructID)
                    && this.assemblyBuilder.IsClosureStruct(closureStructID))) {
                return this.closurePointerField;
            }
        }

        var structID = baseType switch {
            IR.StructType(var id) => id,
            IR.RcPointerType(IR.ClassVirtualTypeID(var id)) => id,

            _ => throw new ArgumentException($"type {baseType} does not have struct fields (accessing field {fieldID.ID})"),
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

    public TypeReference ImportCoreReference(string ns, string name, bool isValueType) {
        var coreLib = this.assemblyBuilder.StandardLibrary;
        var typeRef = new TypeReference(ns, name, coreLib.MainModule, coreLib.Name, isValueType);
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
