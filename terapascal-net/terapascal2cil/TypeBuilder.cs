using System.Diagnostics;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using FieldAttributes = Mono.Cecil.FieldAttributes;
using MethodAttributes = Mono.Cecil.MethodAttributes;
using MethodInfo = System.Reflection.MethodInfo;
using TypeAttributes = Mono.Cecil.TypeAttributes;

namespace Terapascal.CIL;

internal readonly struct LayoutField {
    public FieldReference Field { get; init; }
    public IR.IType Type { get; init; }
}

internal class StructLayout {
    internal required SortedDictionary<IR.FieldID, LayoutField> Fields { get; init; }
}

internal class VariantLayout {
    internal required LayoutField TagField { get; init; }
    internal required List<LayoutField?> Cases { get; init; }
}

internal class BoxTypeInfo {
    internal required TypeReference TypeRef { get; init; }
    internal required FieldReference ValueFieldRef { get; init; }
}

public class TypeBuilder {
    public const string VariantTagName = "Discriminator";

    public const int PointerSize = 8;

    private readonly record struct ArraySig(TypeID Element, ulong Size);

    private readonly AssemblyBuilder assemblyBuilder;

    private readonly TypeCache cache;

    private readonly Dictionary<ArraySig, TypeReference> staticArrayTypes;
    private readonly Dictionary<ArraySig, MethodReference> staticArrayElementMethods;

    private readonly SortedDictionary<TypeID, StructLayout> structFieldMaps;
    private readonly SortedDictionary<TypeID, VariantLayout> variantLayouts;

    private readonly SortedDictionary<TypeID, BoxTypeInfo> boxTypes;

    private readonly Dictionary<(TypeID, IR.MethodID), MethodReference> interfaceMethods;

    private readonly FieldReference closurePointerField;

    private readonly StaticArrayTypeBuilder staticArrayBuilder;

    private readonly SortedDictionary<IR.TypeDefID, string> builtinStructNames;

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

    internal TypeCache Cache => this.cache;

    public TypeBuilder(AssemblyBuilder assemblyBuilder) {
        this.assemblyBuilder = assemblyBuilder;

        this.cache = new TypeCache();

        this.structFieldMaps = new SortedDictionary<TypeID, StructLayout>();
        this.variantLayouts = new SortedDictionary<TypeID, VariantLayout>();

        this.interfaceMethods = new Dictionary<(TypeID, IR.MethodID), MethodReference>();

        this.ValueType = this.ImportCoreReference(typeof(ValueType));
        this.ExceptionType = this.ImportCoreReference(typeof(Exception));
        this.CLRStringType = this.ImportCoreReference(typeof(string));
        this.TypeType = this.ImportCoreReference(typeof(Type));
        this.MethodInfoType = this.ImportCoreReference(typeof(MethodInfo));

        var typeTypeDef = this.ResolveCore(this.TypeType)!;
        this.GetTypeFromHandleMethod = this.ImportCoreReference(typeTypeDef.GetAllMethods()
            .Single(m => m.Name == nameof(Type.GetTypeFromHandle) && m.Parameters.Count == 1));

        var methodInfoTypeDef = this.ResolveCore(this.MethodInfoType)!;
        this.GetMethodFromHandleMethod = this.ImportCoreReference(methodInfoTypeDef.GetAllMethods()
            .Single(m => m.Name == nameof(MethodInfo.GetMethodFromHandle) && m.Parameters.Count == 1));

        this.builtinStructNames = new SortedDictionary<IR.TypeDefID, string> {
            { IR.TypeDefID.String, nameof(Runtime.String) },
            { IR.TypeDefID.TypeInfo, nameof(Runtime.TypeInfo) },
            { IR.TypeDefID.MethodInfo, nameof(Runtime.MethodInfo) },
            { IR.TypeDefID.FunctionInfo, nameof(Runtime.FunctionInfo) },
        };

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

        this.boxTypes = new SortedDictionary<TypeID, BoxTypeInfo>();

        this.staticArrayBuilder = new StaticArrayTypeBuilder(this.assemblyBuilder, this);
        this.staticArrayTypes = new Dictionary<ArraySig, TypeReference>();
        this.staticArrayElementMethods = new Dictionary<ArraySig, MethodReference>();
    }

    public TypeReference BuildTypeRef(IR.IType type, IR.Library library) {
        return this.BuildTypeRef(type, library, out _);
    }

    public TypeReference BuildTypeRef(IR.IType type, IR.Library library, out TypeID typeID) {
        if (this.cache.TryGetType(type, out typeID, out var typeRef)) {
            return typeRef;
        }

        var typeSystem = this.assemblyBuilder.TypeSystem;

        return type switch {
            IR.NothingType => this.RegisterSimpleType(type, typeSystem.Void),
            IR.BoolType => this.RegisterSimpleType(type, typeSystem.Boolean),
            IR.U8Type => this.RegisterSimpleType(type, typeSystem.Byte),
            IR.I8Type => this.RegisterSimpleType(type, typeSystem.SByte),
            IR.U16Type => this.RegisterSimpleType(type, typeSystem.UInt16),
            IR.I16Type => this.RegisterSimpleType(type, typeSystem.Int16),
            IR.U32Type => this.RegisterSimpleType(type, typeSystem.UInt32),
            IR.I32Type => this.RegisterSimpleType(type, typeSystem.Int32),
            IR.U64Type => this.RegisterSimpleType(type, typeSystem.UInt64),
            IR.I64Type => this.RegisterSimpleType(type, typeSystem.Int64),
            IR.USizeType => this.RegisterSimpleType(type, typeSystem.UIntPtr),
            IR.ISizeType => this.RegisterSimpleType(type, typeSystem.IntPtr),
            IR.F32Type => this.RegisterSimpleType(type, typeSystem.Single),
            IR.F64Type => this.RegisterSimpleType(type, typeSystem.Double),
            IR.ArrayType { Element: var element, Length: var length } =>
                this.BuildArrayTypeRef(element, length, library),
            IR.StructType(var structRef) => this.BuildStructDef(structRef, library),
            IR.VariantType(var variantRef) => this.BuildVariantDef(variantRef, library),
            IR.FunctionType(var sig) => this.BuildFunctionTypeDef(sig, library),
            IR.PointerType(var inner) => this.BuildTypeRef(inner, library).MakePointerType(),
            IR.TempRefType(var inner) => this.BuildTypeRef(inner, library).MakeByReferenceType(),
            IR.ObjectType(var id) => this.BuildClassTypeRef(id, library),
            IR.WeakObjectType(var id) => this.BuildClassTypeRef(id, library),
            _ => throw new ArgumentException($"unhandled IR type: {type}"),
        };
    }

    private TypeReference RegisterSimpleType(IR.IType type, TypeReference simpleTypeRef) {
        this.cache.RegisterType(type, simpleTypeRef);
        return simpleTypeRef;
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

    private string CreateUniqueTypeName(IR.NamePath globalName, IR.Metadata metadata, out string ns) {
        var name = globalName.ToGlobalName(out ns);
        
        if (globalName.HasTypeArgs) {
            var argNames = globalName.TypeArgs!.Select(arg => arg.ToPrettyString(metadata));
            name += $"_<{string.Join(",", argNames)}>";
        }

        return name;
    }

    private TypeReference BuildArrayTypeRef(IR.IType element, ulong length, IR.Library library) {
        this.BuildTypeRef(element, library, out var elementID);
        var arraySig = new ArraySig(elementID, length);

        if (this.staticArrayTypes.TryGetValue(arraySig, out var arrayTypeRef)) {
            return arrayTypeRef;
        }

        var id = this.staticArrayTypes.Count;

        var typeDef = this.staticArrayBuilder.BuildArrayTypeRef(element, id, (int)length, library, out var elementMethod);
        this.staticArrayTypes.Add(arraySig, typeDef);
        this.staticArrayElementMethods.Add(arraySig, elementMethod);

        return typeDef;
    }

    private TypeReference BuildClassTypeRef(IR.IObjectID id, IR.Library library) {
        return this.cache.RegisterTypeWith(id.ToObjectType(), typeID => {
            return id switch {
                IR.AnyObjectID => this.assemblyBuilder.Module.TypeSystem.Object,
                IR.ClassObjectID(var classID) => this.BuildStructDef(classID, library),
                IR.InterfaceObjectID(var interfaceID) => this.BuildInterfaceDef(interfaceID, library),
                IR.ClosureObjectID => this.ClosureBaseType,
                IR.ArrayObjectID(var arrayElement) => this.BuildTypeRef(arrayElement, library).MakeArrayType(),
                IR.BoxObjectID(var boxValue) => this.BuildBoxTypeRef(boxValue, library),
                _ => throw new NotImplementedException($"unsupported virtual type ID: {id}"),
            };
        }).TypeRef;
    }

    private TypeReference BuildStructDef(IR.TypeRef structRef, IR.Library library) {
        var structType = structRef.ToStructType();

        if (this.builtinStructNames.TryGetValue(structRef.DefID, out var builtinName)) {
            var builtinTypeRef = this.assemblyBuilder.GetRuntimeTypeRef(builtinName, false);
            var builtinTypeID = this.cache.RegisterType(structType, builtinTypeRef);

            return builtinTypeRef;
        }

        if (!library.Metadata.FindStructDef(structRef.DefID, out var structDef)) {
            throw new InvalidDataException($"missing metadata definition for struct {structRef.DefID}");
        }

        var closureSig = (structDef.Identity as IR.ClosureStructIdentity)?.Identity.Sig;
        var isClass = structDef.Identity is IR.ClassStructIdentity;
        var isValueType = !isClass && closureSig == null;

        TypeReference baseType;
        if (isClass) {
            baseType = this.ObjectBaseType;
        } else if (closureSig != null) {
            baseType = this.ClosureBaseType;
        } else {
            baseType = this.ValueType;
        }

        string ns;
        string name;

        var attrs = TypeAttributes.Sealed
            | TypeAttributes.NotPublic
            | TypeAttributes.AnsiClass
            | TypeAttributes.BeforeFieldInit;
        if (!isValueType) {
            attrs |= TypeAttributes.Class | TypeAttributes.AutoLayout;
        } else {
            attrs |= TypeAttributes.SequentialLayout;
        }

        var declName = structDef.Identity.GetDeclPath();
        if (declName != null) {
            var typeMap = IR.Util.BuildGenericTypeMap(declName.TypeParams ?? [], structRef.Args ?? []);
            var refName = declName.ResolveGeneric(typeMap);

            name = this.CreateUniqueTypeName(refName, library.Metadata, out ns);
        } else {
            name = structDef.Identity.ToPrettyString(library.Metadata);
            ns = "";
        }

        var typeDef = new TypeDefinition(ns, name, attrs, baseType);
        var typeID = this.cache.RegisterType(structType, typeDef);

        Debug.Assert(isValueType == typeDef.IsValueType);

        var fieldLayout = new SortedDictionary<IR.FieldID, LayoutField>();
        this.structFieldMaps[typeID] = new StructLayout {
            Fields = fieldLayout,
        };

        var valueSize = 0;

        foreach (var (fieldID, structFieldDef) in structDef.Fields) {
            var fieldName = structFieldDef.Name ?? $"Field_{fieldID.ID}";

            if (closureSig != null && fieldID.Equals(IR.FieldID.ClosurePointerField)) {
                // the actual pointer field is declared as an IntPtr in the base class
                Debug.Assert(structFieldDef.Type is IR.FunctionType);
                fieldLayout.Add(fieldID, new LayoutField {
                    Field = this.closurePointerField,
                    Type = closureSig.ToFunctionType(),
                });
                continue;
            }

            var fieldType = this.BuildTypeRef(structFieldDef.Type, library);

            var fieldAttrs = FieldAttributes.Assembly;

            var fieldDef = new FieldDefinition(fieldName, fieldAttrs, fieldType);
            typeDef.Fields.Add(fieldDef);

            fieldLayout.Add(fieldID, new LayoutField {
                Field = fieldDef,
                Type = structFieldDef.Type,
            });

            if (isValueType) {
                valueSize += this.GetValueLayoutSize(structFieldDef.Type, library);
            }
        }

        if (isValueType) {
            // value types should be tightly packed by default because it should be up to the
            // frontend to add the correct padding bytes and decide the layout
            typeDef.PackingSize = 1;
            typeDef.ClassSize = valueSize;
        }

        // TODO: native generics
        this.BuildInterfaceImpls(structType, typeDef, library);

        this.BuildDefaultConstructor(typeDef);

        this.assemblyBuilder.Module.Types.Add(typeDef);

        return typeDef;
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

            case IR.StructType(var typeRef): {
                return this.GetStructLayoutSize(typeRef, library);
            }

            case IR.VariantType(var typeRef): {
                return this.GetVariantLayoutSize(typeRef, library);
            }

            default: {
                throw new NotImplementedException($"IR type: {type}");
            }
        }
    }

    private int GetStructLayoutSize(IR.TypeRef typeRef, IR.Library library) {
        if (!library.Metadata.FindStructDef(typeRef.DefID, out var structDef)) {
            throw new InvalidDataException($"type def not found in metadata: struct {typeRef.DefID}");
        }

        // todo: cache this?
        var declPath = structDef.Identity.GetDeclPath();
        if (declPath is { HasTypeParams: true }) {
            var typeMap = IR.Util.BuildGenericTypeMap(declPath.TypeParams, typeRef.Args ?? []);
            structDef = structDef.ResolveGeneric(typeMap);
        }

        // assume structs are tightly packed + padding is handled by the compiler
        var dataSize = 0;
        foreach (var (_, fieldDef) in structDef.Fields) {
            dataSize += this.GetValueLayoutSize(fieldDef.Type, library);
        }

        return dataSize;
    }

    private int GetVariantLayoutSize(IR.TypeRef typeRef, IR.Library library) {
        if (!library.Metadata.FindVariantDef(typeRef.DefID, out var variantDef)) {
            throw new InvalidDataException($"type def not found in metadata: variant {typeRef.DefID}");
        }

        // todo: cache this?
        if (variantDef.Name.HasTypeParams) {
            var typeMap = IR.Util.BuildGenericTypeMap(variantDef.Name.TypeParams, typeRef.Args ?? []);
            variantDef = variantDef.ResolveGeneric(typeMap);
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

    private TypeDefinition BuildVariantDef(IR.TypeRef variantRef, IR.Library library) {
        const TypeAttributes defAttrs = TypeAttributes.Sealed
            | TypeAttributes.NotPublic
            | TypeAttributes.ExplicitLayout;

        var variantType = variantRef.ToVariantType();

        if (!library.Metadata.FindVariantDef(variantRef.DefID, out var variantDef)) {
            throw new InvalidDataException($"missing definition for variant {variantRef.DefID}");
        }

        IR.NamePath variantName;
        if (variantDef.Name.HasTypeParams) {
            var typeMap = IR.Util.BuildGenericTypeMap(variantDef.Name.TypeParams, variantRef.Args ?? []);

            variantDef = variantDef.ResolveGeneric(typeMap);
            variantName = variantDef.Name.ResolveGeneric(typeMap);
        } else {
            variantName = new IR.NamePath { Path = variantDef.Name.Path };
        }

        var (typeID, typeDef) = this.cache.RegisterTypeWith(variantType, typeID => {
            var name = this.CreateUniqueTypeName(variantName, library.Metadata, out var ns);

            var typeDef = new TypeDefinition(ns,
                name,
                defAttrs,
                this.ValueType
            );

            return typeDef;
        });

        var discTypeRef = this.BuildTypeRef(variantDef.TagType, library);
        var discField = new FieldDefinition(VariantTagName, FieldAttributes.Assembly, discTypeRef) {
            Offset = 0,
        };

        typeDef.Fields.Add(discField);

        var caseFieldRefs = new List<LayoutField?>(variantDef.Cases.Count);
        var caseTypeRefs = new (IR.IType defType, TypeReference typeRef)?[variantDef.Cases.Count];

        for (var i = 0; i < variantDef.Cases.Count; i += 1) {
            var dataType = variantDef.Cases[i].Type;
            if (dataType is null or IR.NothingType) {
                continue;
            }

            caseTypeRefs[i] = (dataType, this.BuildTypeRef(dataType, library));
        }

        // it's safe to overlap object references of different types in a union, as long as we use them correctly,
        // which should be enforced by the source language. if there are any object references cases, store them
        // all at the same offset and offset everything else
        var hasObjectMembers = caseTypeRefs.Any(r => r is { typeRef.IsValueType: false });

        var valueDataOffset = hasObjectMembers ? (PointerSize * 2) : PointerSize;

        for (var i = 0; i < variantDef.Cases.Count; i++) {
            var (caseType, dataTypeRef) = caseTypeRefs[i] ?? default;
            if (dataTypeRef is null) {
                caseFieldRefs.Add(null);
                continue;
            }

            var dataField = new FieldDefinition(variantDef.Cases[i].Name, FieldAttributes.Assembly, dataTypeRef) {
                Offset = dataTypeRef.IsValueType ? valueDataOffset : PointerSize,
            };

            typeDef.Fields.Add(dataField);
            caseFieldRefs.Add(new LayoutField {
                Field = dataField,
                Type = caseType,
            });
        }

        // discriminator (pointer sized) + any value members
        typeDef.ClassSize = this.GetVariantLayoutSize(variantDef, library);
        typeDef.PackingSize = 0;

        this.BuildInterfaceImpls(new IR.VariantType(variantRef), typeDef, library);

        this.BuildDefaultConstructor(typeDef);

        this.assemblyBuilder.Module.Types.Add(typeDef);

        this.variantLayouts[typeID] = new VariantLayout {
            TagField = new LayoutField {
                Field = discField,
                Type = variantDef.TagType,
            },
            Cases = caseFieldRefs,
        };

        return typeDef;
    }

    private void BuildInterfaceImpls(IR.IType type, TypeDefinition typeDef, IR.Library library) {
        if (!library.Metadata.InterfaceImpls.TryGetValue(type, out var typeImpls)) {
            return;
        }

        foreach (var ifaceRef in typeImpls.Keys) {
            var interfaceTypeRef = this.BuildTypeRef(ifaceRef.ToObjectID().ToObjectType(), library);

            typeDef.Interfaces.Add(new InterfaceImplementation(interfaceTypeRef));
        }
    }

    private TypeReference BuildBoxTypeRef(IR.IType valueType, IR.Library library) {
        this.BuildTypeRef(valueType, library, out var valueTypeID);

        if (this.boxTypes.TryGetValue(valueTypeID, out var boxInfo)) {
            return boxInfo.TypeRef;
        }

        var attrs = TypeAttributes.Sealed
            | TypeAttributes.NotPublic
            | TypeAttributes.AnsiClass
            | TypeAttributes.BeforeFieldInit
            | TypeAttributes.Class
            | TypeAttributes.AutoLayout;

        var systemTypesNamespace = typeof(Runtime.SystemFunctions).Namespace;
        var typeName = $"Box_{valueType.GetUniqueName(this.cache)}";

        var typeDef = new TypeDefinition(systemTypesNamespace, typeName, attrs, this.ObjectBaseType);

        var valueFieldType = this.BuildTypeRef(valueType, library);
        var valueFieldDef = new FieldDefinition("value", FieldAttributes.Assembly, valueFieldType);
        typeDef.Fields.Add(valueFieldDef);

        this.boxTypes.Add(valueTypeID, new BoxTypeInfo {
            TypeRef = typeDef,
            ValueFieldRef = valueFieldDef,
        });

        this.BuildDefaultConstructor(typeDef);

        this.assemblyBuilder.Module.Types.Add(typeDef);

        return typeDef;
    }

    internal BoxTypeInfo GetBoxTypeInfo(TypeID valueTypeID) {
        return this.boxTypes[valueTypeID];
    }

    private FunctionPointerType BuildFunctionTypeDef(IR.FunctionSig sig, IR.Library library) {
        var pointerType = new FunctionPointerType {
            ReturnType = this.BuildTypeRef(sig.ResultType, library),
        };

        foreach (var paramType in sig.ParameterTypes) {
            var parameterType = this.BuildTypeRef(paramType, library);
            pointerType.Parameters.Add(new ParameterDefinition(parameterType));
        }

        return pointerType;
    }

    private TypeDefinition BuildInterfaceDef(IR.InterfaceRef ifaceRef, IR.Library library) {
        if (!library.Metadata.Interfaces.TryGetValue(ifaceRef.DefID, out var ifaceDecl)
            || ifaceDecl is not IR.DefInterfaceDecl(var ifaceDef)
        ) {
            throw new InvalidDataException($"missing interface definition: {ifaceRef.DefID}");
        }

        var declName = ifaceDecl.GetGlobalName();
        var typeMap = IR.Util.BuildGenericTypeMap(declName.TypeParams ?? [], ifaceRef.Args ?? []);

        var interfaceName = declName.ResolveGeneric(typeMap);

        var name = this.CreateUniqueTypeName(interfaceName, library.Metadata, out var ns);

        const TypeAttributes attrs = TypeAttributes.NotPublic
            | TypeAttributes.Interface
            | TypeAttributes.Abstract
            | TypeAttributes.AutoLayout
            | TypeAttributes.BeforeFieldInit;

        var typeDef = new TypeDefinition(ns, name, attrs);

        var ifaceType = ifaceRef.ToObjectID().ToObjectType();
        var typeID = this.cache.RegisterType(ifaceType, typeDef);

        var module = this.assemblyBuilder.Module;

        for (var methodIndex = 0; methodIndex < ifaceDef.Methods.Count; methodIndex += 1) {
            var ifaceMethod = ifaceDef.Methods[methodIndex];

            var methodAttrs = MethodAttributes.Public
                | MethodAttributes.HideBySig
                | MethodAttributes.Abstract
                | MethodAttributes.Virtual
                | MethodAttributes.NewSlot;

            var selfParam = ifaceMethod.Parameters.FirstOrDefault();

            var hasThis = selfParam != null && ifaceType.Equals(selfParam.Type);
            if (!hasThis) {
                methodAttrs |= MethodAttributes.Static;
            }

            var returnTypeRef = this.assemblyBuilder.TypeBuilder.BuildTypeRef(ifaceMethod.ResultType, library);
            var methodDef = new MethodDefinition(ifaceMethod.Name, methodAttrs, returnTypeRef) {
                HasThis = hasThis,
            };

            // if the method isn't static, the params list implicitly includes the self-type
            var restParams = !methodDef.IsStatic
                ? ifaceMethod.Parameters.Skip(1)
                : ifaceMethod.Parameters;

            foreach (var methodParam in restParams) {
                var paramTypeRef = this.BuildTypeRef(methodParam.Type, library);
                methodDef.Parameters.Add(new ParameterDefinition(paramTypeRef));
            }

            typeDef.Methods.Add(methodDef);

            this.interfaceMethods.Add((typeID, new IR.MethodID((ulong)methodIndex)), methodDef);
        }

        module.Types.Add(typeDef);

        return typeDef;
    }

    public MethodReference GetInterfaceMethod(TypeID ifaceID, IR.MethodID methodID) {
        return this.interfaceMethods[(ifaceID, methodID)];
    }

    public void BuildDefaultConstructor(TypeDefinition typeDef) {
        var voidType = this.assemblyBuilder.TypeSystem.Void;
        var methodDef = new MethodDefinition(
            ".ctor",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.RTSpecialName |
            MethodAttributes.SpecialName,
            voidType
        );

        var body = methodDef.Body.GetILProcessor();

        if (!typeDef.IsValueType) {
            var baseType = this.ResolveCore(typeDef.BaseType) ?? typeDef.BaseType.Resolve();

            var baseCtor = baseType.GetConstructors()
                .Single(ctor => ctor.Parameters.Count == 0);

            var baseCtorRef = this.assemblyBuilder.Module.ImportReference(baseCtor);

            body.Emit(OpCodes.Ldarg_0);
            body.Emit(OpCodes.Call, baseCtorRef);
        }

        body.Emit(OpCodes.Ret);

        typeDef.Methods.Add(methodDef);
    }

    internal LayoutField GetFieldRef(IR.IType baseType, IR.FieldID fieldID, IR.Library library) {
        // the closure field pointer can be accessed either through a closure object pointer
        // (accessing the pointer of an unknown closure type to call it) or directly as a member of a
        // specific closure class (setting the pointer during construction)
        if (fieldID.Equals(IR.FieldID.ClosurePointerField)) {
            if (baseType is IR.ObjectType(IR.ClosureObjectID(var sig))
                || (baseType is IR.StructType(var closureStructID)
                    && this.assemblyBuilder.FindClosureStruct(closureStructID.DefID, out sig))
            ) {
                return new LayoutField {
                    Field = this.closurePointerField,
                    Type = sig.ToFunctionType(),
                };
            }
        }

        this.BuildTypeRef(baseType, library, out var baseTypeID);

        if (!this.structFieldMaps.TryGetValue(baseTypeID, out var structFieldRefs)
            || !structFieldRefs.Fields.TryGetValue(fieldID, out var fieldRef)
        ) {
            var typeDisplay = baseType.ToPrettyString(library.Metadata);

            throw new ArgumentException($"{typeDisplay} does not have field {fieldID.ID}");
        }

        return fieldRef;
    }

    public MethodReference GetStaticArrayElementMethodRef(IR.IType elementType, ulong dim, IR.Library library) {
        this.BuildTypeRef(elementType, library, out var elementTypeID);

        var arraySig = new ArraySig(elementTypeID, dim);

        return this.staticArrayElementMethods[arraySig];
    }

    internal LayoutField GetVariantTagFieldRef(IR.IType baseType, IR.Library library) {
        this.BuildTypeRef(baseType, library, out var baseTypeID);

        if (!this.variantLayouts.TryGetValue(baseTypeID, out var variantLayout)) {
            var typeDisplay = baseType.ToPrettyString(library.Metadata);
            throw new ArgumentException($"variant {typeDisplay} is not defined yet");
        }

        return variantLayout.TagField;
    }

    internal LayoutField GetVariantDataFieldRef(IR.IType baseType, ulong caseIndex, IR.Library library) {
        this.BuildTypeRef(baseType, library, out var baseTypeID);

        var typeDisplay = baseType.ToPrettyString(library.Metadata);

        if (!this.variantLayouts.TryGetValue(baseTypeID, out var variantRefs)) {
            throw new ArgumentException($"variant ID {typeDisplay} is not defined yet");
        }

        return variantRefs.Cases[(int)caseIndex]
            ?? throw new ArgumentException($"invalid index {caseIndex} for variant type {typeDisplay}");
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
