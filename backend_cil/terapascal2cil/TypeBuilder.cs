using System.Diagnostics;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using FieldAttributes = Mono.Cecil.FieldAttributes;
using MethodAttributes = Mono.Cecil.MethodAttributes;
using MethodInfo = System.Reflection.MethodInfo;
using TypeAttributes = Mono.Cecil.TypeAttributes;

namespace Terapascal.CIL;

internal class StructFieldRefs {
    internal required Dictionary<IR.FieldID, FieldReference> FieldRefs { get; init; }
}

internal class VariantFieldRefs {
    internal required FieldReference DiscriminatorFieldRef { get; init; }
    internal required List<FieldReference?> CaseFieldRefs { get; init; }
}

internal class BoxTypeInfo {
    internal required TypeReference TypeRef { get; init; }
    internal required FieldReference ValueFieldRef { get; init; }
}

public class TypeBuilder {
    public const string VariantDiscriminatorName = "Discriminator";

    public const int PointerSize = 8;
    
    private readonly record struct ArraySig(IR.IType Element, ulong Size);

    private readonly AssemblyBuilder assemblyBuilder;

    private readonly Dictionary<IR.IType, TypeReference> cache;

    private readonly Dictionary<ArraySig, TypeReference> staticArrayTypes;
    private readonly Dictionary<ArraySig, MethodReference> staticArrayElementMethods;

    private readonly Dictionary<IR.TypeDefID, FunctionPointerType> funcPointerTypes;

    private readonly Dictionary<IR.TypeDefID, TypeReference> typeDefTypeRefs;
    private readonly Dictionary<IR.InterfaceID, TypeReference> interfaceTypeRefs;

    private readonly Dictionary<IR.TypeDefID, StructFieldRefs> structFields;
    private readonly Dictionary<IR.TypeDefID, VariantFieldRefs> variantFields;

    private readonly Dictionary<IR.IType, BoxTypeInfo> boxTypes;

    private readonly Dictionary<(IR.InterfaceID, IR.MethodID), MethodReference> interfaceMethods;

    private readonly FieldReference closurePointerField;

    private readonly StaticArrayTypeBuilder staticArrayBuilder;

    public TypeReference ExceptionType { get; }
    public TypeReference CLRStringType { get; }
    public TypeReference ValueType { get; }
    public TypeReference TypeType { get; }
    public TypeReference MethodInfoType { get; }
    public TypeReference ErrorType { get; }

    public MethodReference GetTypeFromHandleMethod { get; }
    public MethodReference GetMethodFromHandleMethod { get; }

    public TypeReference ObjectBaseType { get; }
    public MethodReference ObjectCreateMethod { get; }
    public MethodReference ObjectDestroyMethod { get; }
    public MethodReference ObjectIsMethod { get; }
    
    public MethodReference ArrayCreateMethod { get; }

    public TypeReference ClosureBaseType { get; }
    
    public IR.TypeDefID? Flags64StructID { get; private set; }

    public TypeBuilder(AssemblyBuilder assemblyBuilder) {
        this.assemblyBuilder = assemblyBuilder;

        this.cache = new Dictionary<IR.IType, TypeReference>();
        this.funcPointerTypes = new Dictionary<IR.TypeDefID, FunctionPointerType>();

        this.structFields = new Dictionary<IR.TypeDefID, StructFieldRefs>();
        this.variantFields = new Dictionary<IR.TypeDefID, VariantFieldRefs>();

        this.typeDefTypeRefs = new Dictionary<IR.TypeDefID, TypeReference>();
        this.interfaceTypeRefs = new Dictionary<IR.InterfaceID, TypeReference>();
        
        this.interfaceMethods = new Dictionary<(IR.InterfaceID, IR.MethodID), MethodReference>();

        this.ValueType = this.ImportCoreReference(typeof(ValueType));
        this.ExceptionType = this.ImportCoreReference(typeof(Exception));
        this.CLRStringType = this.ImportCoreReference(typeof(string));
        this.TypeType = this.ImportCoreReference(typeof(Type));
        this.MethodInfoType = this.ImportCoreReference(typeof(MethodInfo));

        var typeTypeDef = this.TypeType.Resolve();
        this.GetTypeFromHandleMethod = this.ImportCoreReference(typeTypeDef.GetAllMethods()
            .Single(m => m.Name == nameof(Type.GetTypeFromHandle) && m.Parameters.Count == 1));

        var methodInfoTypeDef = this.MethodInfoType.Resolve();
        this.GetMethodFromHandleMethod = this.ImportCoreReference(methodInfoTypeDef.GetAllMethods()
            .Single(m => m.Name == nameof(MethodInfo.GetMethodFromHandle) && m.Parameters.Count == 1));

        var builtinClasses = (Span<(IR.TypeDefID, string)>)[
            (IR.TypeDefID.String, nameof(Runtime.String)),
            (IR.TypeDefID.TypeInfo, nameof(Runtime.TypeInfo)),
            (IR.TypeDefID.MethodInfo, nameof(Runtime.MethodInfo)), 
            (IR.TypeDefID.FunctionInfo, nameof(Runtime.FunctionInfo)),
        ];

        foreach (var (id, name) in builtinClasses) {
            var typeRef = this.assemblyBuilder.GetRuntimeTypeRef(name, false);
            
            this.cache.Add(id.ToObjectType(), typeRef);
            this.cache.Add(id.ToWeakObjectType(), typeRef);

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

        this.ObjectBaseType = this.assemblyBuilder.GetRuntimeTypeRef(nameof(Runtime.Object), false);
        var objectBaseTypeDef = this.ObjectBaseType.Resolve()
            ?? throw new InvalidDataException($"missing {nameof(Runtime.Object)} base type def in runtime library");

        this.ObjectDestroyMethod = objectBaseTypeDef.Methods.Single(m => m.Name == "Destroy");

        this.ObjectCreateMethod = objectBaseTypeDef.Methods.Single(m => m.Name == "Create");
        this.ObjectIsMethod = objectBaseTypeDef.Methods.Single(m => m.Name == "Is");

        var arrayManagerTypeDef = this.assemblyBuilder.GetRuntimeTypeRef(nameof(Runtime.ArrayManager), false)
                .Resolve()
            ?? throw new InvalidDataException($"missing {nameof(Runtime.ArrayManager)} class in runtime library");

        this.ArrayCreateMethod = arrayManagerTypeDef.Methods.Single(m => 
            m.Name == "CreateArray" && m.GenericParameters.Count == 1);

        this.ErrorType = this.assemblyBuilder.GetRuntimeTypeRef(nameof(Runtime.Error), false);
        
        this.ClosureBaseType = this.assemblyBuilder.GetRuntimeTypeRef(nameof(Runtime.ClosureBase), false);
        var closureTypeDef = this.ClosureBaseType.Resolve()
            ?? throw new InvalidDataException($"missing {nameof(Runtime.ClosureBase)} type def in runtime library");

        this.closurePointerField = closureTypeDef.Fields
            .Single(f => f.Name == nameof(Runtime.ClosureBase.functionPointer));
        this.closurePointerField = this.assemblyBuilder.Module.ImportReference(this.closurePointerField);

        this.boxTypes = new Dictionary<IR.IType, BoxTypeInfo>();

        this.staticArrayBuilder = new StaticArrayTypeBuilder(this.assemblyBuilder, this);
        this.staticArrayTypes = new Dictionary<ArraySig, TypeReference>();
        this.staticArrayElementMethods = new Dictionary<ArraySig, MethodReference>();
    }

    public TypeReference BuildTypeRef(IR.IType type, IR.Library library) {
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
                this.BuildArrayTypeRef(element, length, library),
            IR.StructType(var id) => this.CreateStructTypeRef(id, isValueType: true, library),
            IR.VariantType(var id) => this.CreateStructTypeRef(id, isValueType: true, library),
            IR.FunctionType(var id) => this.BuildFunctionTypeRef(id),
            IR.FlagsType(var id) => this.CreateStructTypeRef(id, isValueType: true, library),
            IR.PointerType(var inner) => this.BuildTypeRef(inner, library).MakePointerType(),
            IR.TempRefType(var inner) => this.BuildTypeRef(inner, library).MakeByReferenceType(),
            IR.ObjectType(var id) => this.BuildClassTypeRef(id, library),
            IR.WeakObjectType(var id) => this.BuildClassTypeRef(id, library),
            _ => throw new ArgumentException($"unhandled IR type: {type}"),
        };

        this.cache.Add(type, typeRef);
        return typeRef;
    }

    public static MethodReference FindCustomAttributeConstructor(TypeDefinition attrTypeDef, params Type[]? paramTypes) {
        return attrTypeDef.GetConstructors()
            .Single(ctor => {
                if (paramTypes == null) {
                    return !ctor.HasParameters;
                }
                
                if (ctor.Parameters.Count != paramTypes.Length) {
                    return false;
                }

                for (var i = 0; i < paramTypes.Length; i += 1) {
                    if (ctor.Parameters[i].ParameterType.FullName != paramTypes[i].FullName) {
                        return false;
                    }
                }

                return true;
            });
    }

    private TypeReference BuildArrayTypeRef(IR.IType element, ulong length, IR.Library library) {
        var arraySig = new ArraySig(element, length);

        if (this.staticArrayTypes.TryGetValue(arraySig, out var arrayTypeRef)) {
            return arrayTypeRef;
        }

        var id = this.staticArrayTypes.Count;

        var typeDef = this.staticArrayBuilder.BuildArrayTypeRef(element, id, (int)length, library, out var elementMethod);
        this.staticArrayTypes.Add(arraySig, typeDef);
        this.staticArrayElementMethods.Add(arraySig, elementMethod);

        return typeDef;
    }

    private TypeReference CreateStructTypeRef(IR.TypeDefID id, bool isValueType, IR.Library library) {
        if (this.typeDefTypeRefs.TryGetValue(id, out var typeRef)) {
            return typeRef;
        }
        
        // the 64-bit flags struct is remapped to a plain U64, and any field instructions on it should be
        // updated accordingly during translation
        if (library.Metadata.FindStructDef(id, out var structDef)
            && structDef.Identity is IR.SetFlagsStructIdentity { Bits: 64 }) {
            this.Flags64StructID = id;
            return this.assemblyBuilder.TypeSystem.UInt64;
        }

        var module = this.assemblyBuilder.Module;
        var ns = this.assemblyBuilder.Assembly.Name.Name;

        typeRef = new TypeReference(ns, $"Struct_{id.ID}", module, module, isValueType);

        this.typeDefTypeRefs.Add(id, typeRef);
        return typeRef;
    }

    private TypeReference BuildInterfaceTypeRef(IR.InterfaceID id) {
        if (this.interfaceTypeRefs.TryGetValue(id, out var ifaceRef)) {
            return ifaceRef;
        }
        
        var module = this.assemblyBuilder.Module;
        var ns = this.assemblyBuilder.Assembly.Name.Name;
        
        ifaceRef = new TypeReference(ns, $"IInterface_{id.ID}", module, module, valueType: false);
        
        this.interfaceTypeRefs.Add(id, ifaceRef);
        return ifaceRef;
    }

    private TypeReference BuildClassTypeRef(IR.IObjectID id, IR.Library library) {
        return id switch {
            IR.AnyObjectID => this.assemblyBuilder.Module.TypeSystem.Object,
            // in this backend, struct refs are automatically reference types if they're a class
            IR.ClassObjectID(var classID) => this.CreateStructTypeRef(classID, false, library),
            IR.InterfaceObjectID(var interfaceID) => this.BuildInterfaceTypeRef(interfaceID),
            IR.ClosureObjectID => this.ClosureBaseType,
            IR.ArrayObjectID(var arrayElement) => this.BuildTypeRef(arrayElement, library).MakeArrayType(),
            IR.BoxObjectID(var boxValue) => this.BuildBoxTypeRef(boxValue, library),
            _ => throw new NotImplementedException($"unsupported virtual type ID: {id}"),
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

    public void BuildStructDef(IR.TypeDefID id, IR.StructDef structDef, IR.Library library) {
        // dynarrays are translated to CLR arrays and don't need a definition
        if (structDef.Identity is IR.DynArrayStructIdentity) {
            return;
        }

        var isClosure = structDef.Identity is IR.ClosureStructIdentity;
        var isClass = structDef.Identity is IR.ClassStructIdentity;
        var isValueType = !isClass && !isClosure;

        var typeRef = this.CreateStructTypeRef(id, isValueType, library);
        
        // creating the type ref will check for the 64-bit struct type which we remap to ulong, so 
        // no need to generate a new type if this is that type
        if (id == this.Flags64StructID) {
            return;
        }

        var attrs = TypeAttributes.Sealed 
            | TypeAttributes.NotPublic
            | TypeAttributes.AnsiClass
            | TypeAttributes.BeforeFieldInit;
        if (!isValueType) {
            attrs |= TypeAttributes.Class | TypeAttributes.AutoLayout;
        } else {
            attrs |= TypeAttributes.SequentialLayout;
        }

        TypeReference baseType;
        if (isClass) {
            baseType = this.ObjectBaseType;
        } else if (isClosure) {
            baseType = this.ClosureBaseType;
        } else {
            baseType = this.ValueType;
        }

        var typeDef = new TypeDefinition(typeRef.Namespace, typeRef.Name, attrs, baseType);

        Debug.Assert(isValueType == typeDef.IsValueType);

        var fieldRefs = new Dictionary<IR.FieldID, FieldReference>();

        var valueSize = 0;
        
        foreach (var (fieldID, structFieldDef) in structDef.Fields) {
            var fieldName = GetFieldName(fieldID);

            if (isClosure && fieldID.Equals(IR.FieldID.ClosurePointerField)) {
                // the actual pointer field is declared as an IntPtr in the base class
                Debug.Assert(structFieldDef.Type is IR.FunctionType);
                fieldRefs.Add(fieldID, this.closurePointerField);
                continue;
            }

            var fieldType = this.BuildTypeRef(structFieldDef.Type, library);

            var fieldAttrs = FieldAttributes.Assembly;

            var fieldDef = new FieldDefinition(fieldName, fieldAttrs, fieldType);
            typeDef.Fields.Add(fieldDef);

            fieldRefs.Add(fieldID, fieldDef);

            if (isValueType) {
                valueSize += this.GetValueLayoutSize(structFieldDef.Type, library);
            }
        }

        this.structFields[id] = new StructFieldRefs {
            FieldRefs = fieldRefs,
        };

        if (!isValueType) {
            this.BuildDefaultConstructor(typeDef);
        } else {
            // value types should be tightly packed by default because it should be up to the 
            // frontend to add the correct padding bytes and decide the layout
            typeDef.PackingSize = 1;
            typeDef.ClassSize = valueSize;
        }

        var interfaceImplAsType = structDef.Identity switch {
            IR.ClassStructIdentity => id.ToObjectType(),
            IR.RecordStructIdentity => id.ToStructType(),
            _ => null,
        };

        if (interfaceImplAsType != null) {
            this.BuildInterfaceImpls(interfaceImplAsType, typeDef, library);
        }

        this.assemblyBuilder.Module.Types.Add(typeDef);
    }

    public int GetValueLayoutSize(IR.IType type, IR.Library library) {
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

            case IR.FlagsType(var defID): {
                return this.GetStructLayoutSize(defID, library);
            }
            case IR.NothingType: {
                return 0;
            }
            
            case IR.ObjectType:
            case IR.WeakObjectType:
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
        var typeRef = this.CreateStructTypeRef(id, isValueType: true, library);

        var typeDef = new TypeDefinition(typeRef.Namespace,
            typeRef.Name,
            TypeAttributes.Sealed | TypeAttributes.NotPublic | TypeAttributes.ExplicitLayout,
            this.ValueType
        );

        var discTypeRef = this.BuildTypeRef(new IR.I32Type(), library);
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
            
            caseTypeRefs[i] = this.BuildTypeRef(dataType, library);
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

        this.BuildInterfaceImpls(new IR.VariantType(id), typeDef, library);
        
        // this.BuildDefaultConstructor(typeDef);

        this.assemblyBuilder.Module.Types.Add(typeDef);
        
        this.variantFields[id] = new VariantFieldRefs {
            DiscriminatorFieldRef = discField,
            CaseFieldRefs = caseFieldRefs,
        };
    }

    private void BuildInterfaceImpls(IR.IType type, TypeDefinition typeDef, IR.Library library) {
        foreach (var (interfaceID, interfaceDecl) in library.Metadata.Interfaces) {
            if (interfaceDecl is not IR.DefInterfaceDecl(var interfaceDef)) {
                continue;
            }

            if (interfaceDef.Implementations.TryGetValue(type, out _)) {
                var interfaceTypeRef = this.BuildInterfaceTypeRef(interfaceID);
                typeDef.Interfaces.Add(new InterfaceImplementation(interfaceTypeRef));
            }
        }
    }

    private TypeReference BuildBoxTypeRef(IR.IType valueType, IR.Library library) {
        if (this.boxTypes.TryGetValue(valueType, out var boxInfo)) {
            return boxInfo.TypeRef;
        }

        var attrs = TypeAttributes.Sealed 
            | TypeAttributes.NotPublic
            | TypeAttributes.AnsiClass
            | TypeAttributes.BeforeFieldInit
            | TypeAttributes.Class 
            | TypeAttributes.AutoLayout;

        var systemTypesNamespace = typeof(Runtime.SystemFunctions).Namespace;
        var typeName = $"Box_{valueType.GetUniqueName()}";

        var typeDef = new TypeDefinition(systemTypesNamespace, typeName, attrs, this.ObjectBaseType);

        var valueFieldType = this.BuildTypeRef(valueType, library);
        var valueFieldDef = new FieldDefinition("value", FieldAttributes.Assembly, valueFieldType);
        typeDef.Fields.Add(valueFieldDef);

        this.boxTypes.Add(valueType, new BoxTypeInfo {
            TypeRef = typeDef,
            ValueFieldRef = valueFieldDef,
        });

        this.BuildDefaultConstructor(typeDef);

        this.assemblyBuilder.Module.Types.Add(typeDef);

        return typeDef;
    }

    internal BoxTypeInfo GetBoxTypeInfo(IR.IType valueType) {
        return this.boxTypes[valueType];
    }

    public void BuildFunctionTypeDef(IR.TypeDefID id, IR.FunctionSig sig, IR.Library library) {
        var pointerType = new FunctionPointerType {
            ReturnType = this.BuildTypeRef(sig.ReturnType, library),
        };

        foreach (var paramType in sig.ParameterTypes) {
            var parameterType = this.BuildTypeRef(paramType, library);
            pointerType.Parameters.Add(new ParameterDefinition(parameterType));
        }
        
        this.funcPointerTypes.Add(id, pointerType);
    }

    public FunctionPointerType GetFunctionPointerType(IR.TypeDefID id) {
        return this.funcPointerTypes[id];
    }

    public void BuildInterfaceDef(IR.InterfaceID id, IR.InterfaceDef def, IR.Library library) {
        var module = this.assemblyBuilder.Module;

        const TypeAttributes attrs = TypeAttributes.NotPublic
            | TypeAttributes.Interface
            | TypeAttributes.Abstract
            | TypeAttributes.AutoLayout
            | TypeAttributes.BeforeFieldInit;

        var ifaceTypeRef = this.BuildInterfaceTypeRef(id);

        var ifaceDef = new TypeDefinition(ifaceTypeRef.Namespace, ifaceTypeRef.Name, attrs);
        var ifaceSelfType = new IR.ObjectType(new IR.InterfaceObjectID(id));

        for (var methodIndex = 0; methodIndex < def.Methods.Count; methodIndex += 1) {
            var ifaceMethod = def.Methods[methodIndex];

            var methodAttrs = MethodAttributes.Public 
                | MethodAttributes.HideBySig 
                | MethodAttributes.Abstract
                | MethodAttributes.Virtual
                | MethodAttributes.NewSlot;

            if (!ifaceSelfType.Equals(ifaceMethod.Parameters.FirstOrDefault())) {
                methodAttrs |= MethodAttributes.Static;
            }

            var returnTypeRef = this.assemblyBuilder.TypeBuilder.BuildTypeRef(ifaceMethod.ReturnType, library);
            var methodDef = new MethodDefinition(ifaceMethod.Name, methodAttrs, returnTypeRef);
            methodDef.HasThis = true;
            
            // if the method isn't static, the params list implicitly includes the self-type 
            var restParams = !methodDef.IsStatic 
                ? ifaceMethod.Parameters.Skip(1) 
                : ifaceMethod.Parameters; 

            foreach (var paramType in restParams) {
                var paramTypeRef = this.assemblyBuilder.TypeBuilder.BuildTypeRef(paramType, library);
                methodDef.Parameters.Add(new ParameterDefinition(paramTypeRef));
            }

            ifaceDef.Methods.Add(methodDef);

            this.interfaceMethods.Add((id, new IR.MethodID((ulong)methodIndex)), methodDef);
        }

        module.Types.Add(ifaceDef);
    }

    public MethodReference GetInterfaceMethod(IR.InterfaceID ifaceID, IR.MethodID methodID) {
        return this.interfaceMethods[(ifaceID, methodID)];
    }

    private void BuildDefaultConstructor(TypeDefinition typeDef) {
        var voidType = this.assemblyBuilder.TypeSystem.Void;
        var methodDef = new MethodDefinition(
            ".ctor",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.RTSpecialName |
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

    public FieldReference GetFieldRef(IR.IType baseType, IR.FieldID fieldID, IR.Library library) {
        // the closure field pointer can be accessed either through a closure object pointer
        // (accessing the pointer of an unknown closure type to call it) or directly as a member of a
        // specific closure class (setting the pointer during construction)
        if (fieldID.Equals(IR.FieldID.ClosurePointerField)) {
            if (baseType is IR.ObjectType(IR.ClosureObjectID)
                || (baseType is IR.StructType(var closureStructID)
                    && this.assemblyBuilder.IsClosureStruct(closureStructID))) {
                return this.closurePointerField;
            }
        }

        var structID = baseType switch {
            IR.StructType(var id) => id,
            IR.FlagsType(var id) => id,
            IR.ObjectType(IR.ClassObjectID(var id)) => id,

            _ => throw new ArgumentException($"type {baseType} does not have struct fields (accessing field {fieldID.ID})"),
        };

        if (!this.structFields.TryGetValue(structID, out var structFieldRefs)
            || !structFieldRefs.FieldRefs.TryGetValue(fieldID, out var fieldRef)) {
            throw new ArgumentException($"struct ID {structID.ID} ({baseType}) does not have field {fieldID.ID}");
        }

        return fieldRef;
    }

    public MethodReference GetStaticArrayElementMethodRef(IR.IType elementType, ulong dim) {
        var arraySig = new ArraySig(elementType, dim);

        return this.staticArrayElementMethods[arraySig];
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

    public GenericInstanceMethod GetObjectCreateMethod(IR.IObjectID classID, IR.Library library) {
        var module = this.assemblyBuilder.Module;
        
        var classTypeRef = this.BuildTypeRef(new IR.ObjectType(classID), library);

        var methodInstance = new GenericInstanceMethod(module.ImportReference(this.ObjectCreateMethod));
        methodInstance.GenericArguments.Add(module.ImportReference(classTypeRef));

        return methodInstance;
    }

    public GenericInstanceMethod GetArrayCreateMethod(IR.IType elementType, IR.Library library) {
        var module = this.assemblyBuilder.Module;
        
        var elementTypeRef = this.BuildTypeRef(elementType, library);

        var methodInstance = new GenericInstanceMethod(module.ImportReference(this.ArrayCreateMethod));
        methodInstance.GenericArguments.Add(module.ImportReference(elementTypeRef));

        return methodInstance;
    }

    public TypeReference ImportCoreReference(string? ns, string name, bool isValueType) {
        var coreLib = this.assemblyBuilder.StandardLibrary;
        var typeRef = new TypeReference(ns, name, coreLib.MainModule, coreLib.Name, isValueType);
        typeRef = coreLib.MainModule.ImportReference(typeRef);
        typeRef = this.assemblyBuilder.Module.ImportReference(typeRef);

        return typeRef;
    }
    
    public TypeReference ImportCoreReference(Type type) {
        return this.ImportCoreReference(type.Namespace, type.Name, type.IsValueType);
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
