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

internal class InterfaceTypeInstance {
    internal required IR.InterfaceDef InterfaceDef { get; init; }
    internal required TypeDefinition TypeDef { get; init; }
}

public class TypeBuilder {
    public const string VariantTagName = "Discriminator";

    public const int PointerSize = 8;

    private readonly record struct ArraySig(TypeID Element, ulong Size);

    private readonly AssemblyBuilder assemblyBuilder;

    private readonly TypeCache cache;

    private readonly Dictionary<ArraySig, MethodReference> staticArrayElementMethods;

    private readonly SortedDictionary<TypeID, StructLayout> structLayouts;
    private readonly SortedDictionary<TypeID, VariantLayout> variantLayouts;

    private readonly SortedDictionary<TypeID, InterfaceTypeInstance> interfaceTypeCache;
    private readonly Dictionary<(TypeID, IR.MethodID), MethodReference> interfaceMethods;

    private readonly FieldReference closurePointerField;

    private readonly StaticArrayTypeBuilder staticArrayBuilder;

    private readonly SortedDictionary<IR.TypeDefID, string> builtinStructNames;

    private readonly SortedDictionary<TypeID, (MethodReference Retain, MethodReference Release)> rcMethodTables;

    private readonly TypeDefinition rcActionTypeDef;
    private readonly MethodReference registerRcActionsMethodRef;

    public TypeReference ExceptionType { get; }
    public TypeReference CLRStringType { get; }
    public TypeReference ValueType { get; }
    public TypeReference TypeType { get; }
    public TypeReference TypedReferenceType { get; }
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

        this.structLayouts = [];
        this.variantLayouts = [];

        this.interfaceTypeCache = [];
        this.interfaceMethods = [];

        this.rcMethodTables = [];

        this.staticArrayElementMethods = [];

        this.ValueType = this.ImportCoreReference(typeof(ValueType));
        this.ExceptionType = this.ImportCoreReference(typeof(Exception));
        this.CLRStringType = this.ImportCoreReference(typeof(string));
        this.TypeType = this.ImportCoreReference(typeof(Type));
        this.TypedReferenceType = this.ImportCoreReference(typeof(TypedReference));
        this.MethodInfoType = this.ImportCoreReference(typeof(MethodInfo));

        this.rcActionTypeDef = this.assemblyBuilder.GetRuntimeTypeRef("RcMethod", false).Resolve();

        this.registerRcActionsMethodRef = this.assemblyBuilder.FindRuntimeFunction("RegisterRcMethodTable");

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

        this.staticArrayBuilder = new StaticArrayTypeBuilder(this.assemblyBuilder, this);
    }

    public TypeReference BuildType(IR.IType type) {
        return this.BuildType(type, out _);
    }

    public TypeReference BuildType(IR.IType type, out TypeID typeID) {
        if (this.cache.TryGetType(type, out typeID, out var typeRef)) {
            return typeRef;
        }

        var typeSystem = this.assemblyBuilder.TypeSystem;

        typeRef = type switch {
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
                this.BuildArrayTypeRef(element, length),
            IR.StructType(var structRef) => this.BuildStructDef(structRef),
            IR.VariantType(var variantRef) => this.BuildVariantDef(variantRef),
            IR.FunctionType(var sig) => this.BuildFunctionTypeDef(sig),
            IR.PointerType(var inner) => this.RegisterSimpleType(type, this.BuildType(inner).MakePointerType()),
            IR.TempRefType(var inner) => this.RegisterSimpleType(type, this.BuildType(inner).MakeByReferenceType()),
            IR.ObjectType(var id) => this.BuildClassTypeRef(id),
            IR.WeakObjectType(var id) => this.BuildClassTypeRef(id),
            _ => throw new ArgumentException($"unhandled IR type: {type}"),
        };

        typeID = this.cache.GetTypeID(type);

        return typeRef;
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

    private string CreateUniqueTypeName(IR.NamePath globalName, out string ns) {
        var name = globalName.ToGlobalName(out ns);
        
        if (globalName.HasTypeArgs) {
            var argNames = globalName.TypeArgs!.Select(arg => arg.ToString(this.assemblyBuilder.LoadedMetadata));
            name += $"_<{string.Join(",", argNames)}>";
        }

        return name;
    }

    private TypeReference BuildArrayTypeRef(IR.IType element, ulong length) {
        this.BuildType(element, out var elementID);
        var arraySig = new ArraySig(elementID, length);

        var typeDef = this.staticArrayBuilder.BuildArrayTypeRef(element, length, out var elementMethod);
        this.staticArrayElementMethods.Add(arraySig, elementMethod);

        return typeDef;
    }

    private TypeReference BuildClassTypeRef(IR.IObjectID id) {
        var classTypeRef = id switch {
            IR.AnyObjectID => this.assemblyBuilder.Module.TypeSystem.Object,
            IR.ClassObjectID(var classID) => this.BuildStructDef(classID),
            IR.InterfaceObjectID(var interfaceID) => this.BuildInterfaceDef(interfaceID),
            IR.AnyClosureObjectID => this.ClosureBaseType,
            IR.ArrayObjectID(var arrayElement) => this.BuildType(arrayElement).MakeArrayType(),
            IR.BoxObjectID(var boxValue) => this.BuildBoxTypeRef(boxValue),
            _ => throw new NotImplementedException($"unsupported virtual type ID: {id}"),
        };

        this.cache.RegisterType(id.ToObjectType(), classTypeRef);
        this.cache.RegisterType(id.ToWeakObjectType(), classTypeRef);
        return classTypeRef;
    }

    private TypeReference BuildStructDef(IR.TypeRef structRef) {
        if (!this.assemblyBuilder.LoadedMetadata.FindStructDef(structRef.DefID, out var structDef)) {
            throw new InvalidDataException($"missing metadata definition for struct {structRef.DefID}");
        }

        var closureSig = (structDef.Identity as IR.ClosureStructIdentity)?.Identity.Sig;
        var isClass = structDef.Identity is IR.ClassStructIdentity;
        var isValueType = !isClass && closureSig == null;

        TypeReference baseType;
        IR.IType structType;

        if (isClass) {
            baseType = this.ObjectBaseType;
            structType = structRef.ToClassObjectID().ToObjectType();
        } else if (closureSig != null) {
            baseType = this.ClosureBaseType;
            structType = structRef.ToClassObjectID().ToObjectType();
        } else {
            baseType = this.ValueType;
            structType = structRef.ToStructType();
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

            structDef = structDef.ResolveGeneric(typeMap);

            name = this.CreateUniqueTypeName(refName, out ns);
        } else {
            name = structDef.Identity.ToString(this.assemblyBuilder.LoadedMetadata);
            ns = "";
        }

        TypeReference typeRef;
        if (this.builtinStructNames.TryGetValue(structRef.DefID, out var builtinName)) {
            var builtinTypeDef = this.assemblyBuilder.GetRuntimeTypeRef(builtinName, false).Resolve();

            typeRef = this.assemblyBuilder.Module.ImportReference(builtinTypeDef);

            var builtinTypeID = this.cache.RegisterType(structType, typeRef);

            this.BuildStructLayoutFromBuiltinType(builtinTypeDef, builtinTypeID, structDef);
        } else {
            var typeDef = new TypeDefinition(ns, name, attrs, baseType);
            this.assemblyBuilder.Module.Types.Add(typeDef);

            typeRef = typeDef;

            var typeID = this.cache.RegisterType(structType, typeRef);

            this.BuildStructLayoutFromDef(typeID, typeDef, structDef, closureSig);

            // TODO: native generics
            this.BuildInterfaceImpls(structType, typeDef);

            this.BuildDefaultConstructor(typeDef);

            if (isValueType) {
                this.BuildStructRCMethods(typeID, structDef, typeDef);
            } else {
                this.BuildClassDestroyMethod(typeID, typeDef, structType);
            }
        }

        Debug.Assert(isValueType == typeRef.IsValueType);

        return typeRef;
    }

    private bool OverridesObjectDestroyMethod(MethodDefinition methodDef) {
        if (methodDef.MetadataToken.Equals(this.ObjectDestroyMethod.MetadataToken)) {
            return true;
        }

        if (!methodDef.HasOverrides) {
            return false;
        }

        foreach (var overrideMethod in methodDef.Overrides) {
            if (this.OverridesObjectDestroyMethod(overrideMethod.Resolve())) {
                return true;
            }
        }

        return false;
    }

    private MethodDefinition FindClassDestroyMethod(TypeDefinition typeDef) {
        while (true) {
            foreach (var method in typeDef.Methods) {
                if (this.OverridesObjectDestroyMethod(method)) {
                    return method;
                }
            }

            if (typeDef.BaseType != null) {
                typeDef = typeDef.BaseType.Resolve();
            } else {
                var err = $"class objects must descend from the base Object type (couldn't find Destroy method in {typeDef})";
                throw new InvalidOperationException(err);
            }
        }
    }

    private void BuildClassDestroyMethod(
        TypeID typeID,
        TypeDefinition typeDef,
        IR.IType classType
    ) {
        var metadata = this.assemblyBuilder.LoadedMetadata;

        var baseTypeDef = typeDef.BaseType.Resolve();
        var baseDestroy = this.assemblyBuilder.Module.ImportReference(this.FindClassDestroyMethod(baseTypeDef));

        var baseDestroyDef = baseDestroy.Resolve();

        var attrs = baseDestroyDef.Attributes & ~MethodAttributes.NewSlot;

        var destroyMethod = new MethodDefinition(baseDestroy.Name, attrs, baseDestroyDef.ReturnType);
        destroyMethod.Overrides.Add(baseDestroy);

        typeDef.Methods.Add(destroyMethod);

        var destroyBody = destroyMethod.Body.GetILProcessor();

        // call user destructor
        if (metadata.FindDestructor(classType, out var dtorFunc)) {
            destroyBody.Emit(OpCodes.Ldarg_0);
            destroyBody.Emit(OpCodes.Call, baseDestroy);

            var dtorTypeArgs = classType.GetTypeRef()?.Args ?? [];
            var dtorMethod = this.assemblyBuilder.FunctionBuilder.GetFunctionMethod(dtorFunc.ToFunctionRef(dtorTypeArgs));

            destroyBody.Emit(OpCodes.Ldarg_0);
            destroyBody.Emit(OpCodes.Call, dtorMethod);
        }

        // release fields
        var classLayout = this.structLayouts[typeID];
        var releaseFunc = this.assemblyBuilder.FindRuntimeFunction(nameof(Runtime.SystemFunctions.RcRelease));

        foreach (var (_, layoutField) in classLayout.Fields) {
            if (!metadata.TypeContainsObjectRefs(layoutField.Type)) {
                continue;
            }

            // call the system release/retain func on an object ref
            var releaseInstance = new GenericInstanceMethod(releaseFunc);
            releaseInstance.GenericArguments.Add(layoutField.Field.FieldType);

            // load field ref
            destroyBody.Emit(OpCodes.Ldarg_0);
            destroyBody.Emit(OpCodes.Ldflda, layoutField.Field);

            // load weak arg
            destroyBody.Emit(layoutField.Type is IR.WeakObjectType ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);

            // Release(ref field, fieldType is weak)
            destroyBody.Emit(OpCodes.Call, releaseInstance);
        }

        destroyBody.Emit(OpCodes.Ret);
    }

    private void BuildStructRCMethods(
        TypeID typeID,
        IR.StructDef structDef,
        TypeDefinition typeDef
    ) {
        var metadata = this.assemblyBuilder.LoadedMetadata;

        if (!structDef.Fields.Values.Any(fieldDef => metadata.TypeContainsObjectRefs(fieldDef.Type))) {
            return;
        }

        var retainMethodDef = this.BuildStructRCMethod(typeDef, typeID, retain: true);
        var releaseMethodDef = this.BuildStructRCMethod(typeDef, typeID, retain: false);

        this.rcMethodTables.Add(typeID, (retainMethodDef, releaseMethodDef));

        this.RegisterRCMethodTable(typeDef, retainMethodDef, releaseMethodDef);
    }

    private void BuildVariantRCMethods(
        TypeID typeID,
        IR.VariantDef variantDef,
        TypeDefinition typeDef
    ) {
        var metadata = this.assemblyBuilder.LoadedMetadata;

        if (!variantDef.Cases.Any(c => c.Type != null && metadata.TypeContainsObjectRefs(c.Type))) {
            return;
        }

        var retainMethodDef = this.BuildVariantRCMethod(typeDef, typeID, variantDef, retain: true);
        var releaseMethodDef = this.BuildVariantRCMethod(typeDef, typeID, variantDef, retain: false);

        this.rcMethodTables.Add(typeID, (retainMethodDef, releaseMethodDef));

        this.RegisterRCMethodTable(typeDef, retainMethodDef, releaseMethodDef);
    }

    private MethodDefinition BuildVariantRCMethod(
        TypeDefinition variantTypeDef,
        TypeID variantTypeID,
        IR.VariantDef variantDef,
        bool retain
    ) {
        var methodDef = this.CreateRCMethod(variantTypeDef, retain);

        var body = methodDef.Body.GetILProcessor();

        var variantLayout = this.variantLayouts[variantTypeID];

        var systemFunc = this.assemblyBuilder.FindRuntimeFunction(retain
            ? nameof(Runtime.SystemFunctions.RcRetain)
            : nameof(Runtime.SystemFunctions.RcRelease));

        this.VisitVariantCurrentCase(
            methodDef,
            variantTypeDef,
            variantDef,
            variantLayout,
            field => this.assemblyBuilder.LoadedMetadata.TypeContainsObjectRefs(field.Type),
            field => {
                // call the system release/retain func on an object ref
                var systemFuncInstance = new GenericInstanceMethod(systemFunc);
                systemFuncInstance.GenericArguments.Add(field.Field.FieldType);

                var weak = field.Type is IR.WeakObjectType;
                body.Emit(weak ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
                body.Emit(OpCodes.Call, systemFuncInstance);
            });

        body.Emit(OpCodes.Ret);

        return methodDef;
    }

    private void VisitVariantCurrentCase(
        MethodDefinition methodDef,
        TypeReference variantTypeRef,
        IR.VariantDef variantDef,
        VariantLayout variantLayout,
        Func<LayoutField, bool> predicate,
        Action<LayoutField> emit
    ) {
        var body = methodDef.Body.GetILProcessor();

        var selfRefVar = new VariableDefinition(variantTypeRef.MakeByReferenceType());
        methodDef.Body.Variables.Add(selfRefVar);

        // cast object pointer to a ref to the struct
        body.Emit(OpCodes.Ldarg_0);
        body.Emit(OpCodes.Refanyval, variantTypeRef);
        body.Emit(OpCodes.Stloc, selfRefVar);

        var breakLabel = body.Create(OpCodes.Nop);

        for (var i = 0; i < variantDef.Cases.Count; i += 1) {
            var defCase = variantDef.Cases[i];
            var layoutCase = variantLayout.Cases[i];

            if (defCase.Type == null
                || !defCase.Tag.ToLiteralValue(out var caseTagVal)
                || layoutCase == null
                || !predicate(layoutCase.Value)) {
                continue;
            }

            // compare case tag to active tag value
            var falseLabel = body.Create(OpCodes.Nop);

            body.Emit(OpCodes.Ldloc, selfRefVar);
            body.Emit(OpCodes.Ldfld, variantLayout.TagField.Field);
            body.EmitLoadLiteral(defCase.Tag);

            body.Emit(OpCodes.Ceq);
            body.Emit(OpCodes.Brfalse, falseLabel);

            body.Emit(OpCodes.Ldloc, selfRefVar);
            body.Emit(OpCodes.Ldflda, layoutCase.Value.Field);
            emit(layoutCase.Value);

            body.Emit(OpCodes.Br, breakLabel);

            body.Append(falseLabel);
        }

        body.Append(breakLabel);

        body.Emit(OpCodes.Ret);
    }

    internal void RegisterRCMethodTable(
        TypeReference typeRef,
        MethodReference retainMethodRef,
        MethodReference releaseMethodRef
    ) {
        var module = this.assemblyBuilder.Module;

        var rcActionTypeRef = module.ImportReference(this.rcActionTypeDef);

        var rcActionCtor = this.rcActionTypeDef
                .FindConstructor(isStatic: false, [typeof(object), typeof(IntPtr)])
            ?? throw new InvalidOperationException("unable to find constructor for RC delegate type");

        rcActionCtor = module.ImportReference(rcActionCtor);

        var cctor = this.assemblyBuilder.GetInternalClass().GetOrCreateCCtor();
        var cctorBody = cctor.Body.GetILProcessor();

        // if there's already a cctor, we're appending to it, so remove the ret for now
        var retainerVar = new VariableDefinition(rcActionTypeRef);
        var releaserVar = new VariableDefinition(rcActionTypeRef);

        cctor.Body.Variables.Add(retainerVar);
        cctor.Body.Variables.Add(releaserVar);

        cctorBody.Emit(OpCodes.Ldnull);
        cctorBody.Emit(OpCodes.Ldftn, retainMethodRef);
        cctorBody.Emit(OpCodes.Newobj, rcActionCtor);
        cctorBody.Emit(OpCodes.Stloc, retainerVar);

        cctorBody.Emit(OpCodes.Ldnull);
        cctorBody.Emit(OpCodes.Ldftn, releaseMethodRef);
        cctorBody.Emit(OpCodes.Newobj, rcActionCtor);
        cctorBody.Emit(OpCodes.Stloc, releaserVar);

        var registerRcInstance = new GenericInstanceMethod(this.registerRcActionsMethodRef) {
            GenericArguments = { typeRef },
        };

        cctorBody.Emit(OpCodes.Ldloc, retainerVar);
        cctorBody.Emit(OpCodes.Ldloc, releaserVar);
        cctorBody.Emit(OpCodes.Call, registerRcInstance);
    }

    internal MethodDefinition CreateRCMethod(TypeDefinition typeDef, bool retain) {
        var typeSystem = this.assemblyBuilder.TypeSystem;

        const MethodAttributes attrs = MethodAttributes.HideBySig
            | MethodAttributes.SpecialName
            | MethodAttributes.Assembly
            | MethodAttributes.Static;

        var actionName = retain ? "retain" : "release";

        var methodDef = new MethodDefinition($"__<{actionName}>", attrs, typeSystem.Void);
        typeDef.Methods.Add(methodDef);

        methodDef.Parameters.Add(new ParameterDefinition(this.TypedReferenceType) {
            Name = "self",
        });

        return methodDef;
    }

    private MethodDefinition BuildStructRCMethod(
        TypeDefinition structTypeDef,
        TypeID structTypeID,
        bool retain
    ) {
        var methodDef = this.CreateRCMethod(structTypeDef, retain);

        var body = methodDef.Body.GetILProcessor();

        var structLayout = this.structLayouts[structTypeID];

        var systemFunc = this.assemblyBuilder.FindRuntimeFunction(retain
            ? nameof(Runtime.SystemFunctions.RcRetain)
            : nameof(Runtime.SystemFunctions.RcRelease));

        VisitStructFields(
            methodDef,
            structTypeDef,
            structLayout,
            field => this.assemblyBuilder.LoadedMetadata.TypeContainsObjectRefs(field.Type),
            field => {
                // call the system release/retain func on an object ref
                var systemFuncInstance = new GenericInstanceMethod(systemFunc);
                systemFuncInstance.GenericArguments.Add(field.Field.FieldType);

                var weak = field.Type is IR.WeakObjectType;
                body.Emit(weak ? OpCodes.Ldc_I4_1 : OpCodes.Ldc_I4_0);
                body.Emit(OpCodes.Call, systemFuncInstance);
            });

        body.Emit(OpCodes.Ret);

        return methodDef;
    }

    private static void VisitStructFields(
        MethodDefinition methodDef,
        TypeReference structTypeRef,
        StructLayout structLayout,
        Func<LayoutField, bool> predicate,
        Action<LayoutField> emitVisit
    ) {
        var body = methodDef.Body.GetILProcessor();

        var selfRefVar = new VariableDefinition(structTypeRef.MakeByReferenceType());
        methodDef.Body.Variables.Add(selfRefVar);

        // cast object pointer to a ref to the struct
        body.Emit(OpCodes.Ldarg_0);
        body.Emit(OpCodes.Refanyval, structTypeRef);
        body.Emit(OpCodes.Stloc, selfRefVar);

        foreach (var (_, layoutField) in structLayout.Fields) {
            if (!predicate(layoutField)) {
                continue;
            }

            body.Emit(OpCodes.Ldloc, selfRefVar);
            body.Emit(OpCodes.Ldflda, layoutField.Field);

            emitVisit(layoutField);
        }
    }

    private void BuildStructLayoutFromDef(
        TypeID typeID,
        TypeDefinition typeDef,
        IR.StructDef structDef,
        IR.FunctionSig? closureSig
    ) {
        var fieldLayout = new SortedDictionary<IR.FieldID, LayoutField>();
        this.structLayouts[typeID] = new StructLayout {
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

            var fieldType = this.BuildType(structFieldDef.Type);

            var fieldAttrs = FieldAttributes.Assembly;

            var fieldDef = new FieldDefinition(fieldName, fieldAttrs, fieldType);
            typeDef.Fields.Add(fieldDef);

            fieldLayout.Add(fieldID, new LayoutField {
                Field = fieldDef,
                Type = structFieldDef.Type,
            });

            if (typeDef.IsValueType) {
                valueSize += this.GetValueLayoutSize(structFieldDef.Type);
            }
        }

        if (typeDef.IsValueType && structDef.Layout is IR.StructLayout.Packed) {
            // value types should be tightly packed by default because it should be up to the
            // frontend to add the correct padding bytes and decide the layout
            typeDef.PackingSize = 1;
            typeDef.ClassSize = valueSize;
        }
    }

    private void BuildStructLayoutFromBuiltinType(
        TypeDefinition builtinType,
        TypeID typeID,
        IR.StructDef structDef
    ) {
        var fieldLayout = new SortedDictionary<IR.FieldID, LayoutField>();
        this.structLayouts[typeID] = new StructLayout {
            Fields = fieldLayout,
        };

        foreach (var (fieldID, fieldDef) in structDef.Fields) {
            // expect the fields to appear in the exact same order as they do in the native def
            var nativeField = builtinType.Fields[(int)fieldID.ID];

            fieldLayout.Add(fieldID, new LayoutField {
                Field = this.assemblyBuilder.Module.ImportReference(nativeField),
                Type = fieldDef.Type,
            });
        }
    }

    public int GetValueLayoutSize(IR.IType type) {
        switch (type) {
            case IR.ArrayType arrayType: {
                var elementSize = this.GetValueLayoutSize(arrayType.Element);
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
                return this.GetStructLayoutSize(typeRef);
            }

            case IR.VariantType(var typeRef): {
                return this.GetVariantLayoutSize(typeRef);
            }

            default: {
                throw new NotImplementedException($"IR type: {type}");
            }
        }
    }

    private int GetStructLayoutSize(IR.TypeRef typeRef) {
        if (!this.assemblyBuilder.LoadedMetadata.FindStructDef(typeRef.DefID, out var structDef)) {
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
            dataSize += this.GetValueLayoutSize(fieldDef.Type);
        }

        return dataSize;
    }

    private int GetVariantLayoutSize(IR.TypeRef typeRef) {
        if (!this.assemblyBuilder.LoadedMetadata.FindVariantDef(typeRef.DefID, out var variantDef)) {
            throw new InvalidDataException($"type def not found in metadata: variant {typeRef.DefID}");
        }

        // todo: cache this?
        if (variantDef.Name.HasTypeParams) {
            var typeMap = IR.Util.BuildGenericTypeMap(variantDef.Name.TypeParams, typeRef.Args ?? []);
            variantDef = variantDef.ResolveGeneric(typeMap);
        }

        return this.GetVariantLayoutSize(variantDef);
    }

    private int GetVariantLayoutSize(IR.VariantDef variantDef) {
        var hasObjectData = false;
        var maxDataSize = 0;
        foreach (var caseDef in variantDef.Cases) {
            if (caseDef.Type == null) {
                continue;
            }

            if (caseDef.Type.IsObjectType()) {
                hasObjectData = true;
            } else {
                maxDataSize += this.GetValueLayoutSize(caseDef.Type);
            }
        }

        var totalSize = PointerSize + maxDataSize;
        if (hasObjectData) {
            totalSize += PointerSize;
        }

        return totalSize;
    }

    private TypeDefinition BuildVariantDef(IR.TypeRef variantRef) {
        const TypeAttributes defAttrs = TypeAttributes.Sealed
            | TypeAttributes.NotPublic
            | TypeAttributes.ExplicitLayout;

        var variantType = variantRef.ToVariantType();

        var metadata = this.assemblyBuilder.LoadedMetadata;

        if (!metadata.FindVariantDef(variantRef.DefID, out var variantDef)) {
            throw new InvalidDataException($"missing definition for variant {variantRef.DefID}");
        }

        var tagAttrCtor = this.assemblyBuilder
                .GetRuntimeTypeRef(nameof(Runtime.VariantTagFieldAttribute), false)
                .Resolve()?
                .FindConstructor(isStatic: false, [])
            ?? throw new InvalidOperationException("missing constructor def for variant tag field attribute type");
        tagAttrCtor = this.assemblyBuilder.Module.ImportReference(tagAttrCtor);

        var caseAttrCtor = this.assemblyBuilder
                .GetRuntimeTypeRef(nameof(Runtime.VariantCaseFieldAttribute), false)
                .Resolve()?
                .FindConstructor(isStatic: false, [typeof(object)])
            ?? throw new InvalidOperationException("missing constructor def for variant case field attribute type");
        caseAttrCtor = this.assemblyBuilder.Module.ImportReference(caseAttrCtor);

        IR.NamePath variantName;
        if (variantDef.Name.HasTypeParams) {
            var typeMap = IR.Util.BuildGenericTypeMap(variantDef.Name.TypeParams, variantRef.Args ?? []);

            variantDef = variantDef.ResolveGeneric(typeMap);
            variantName = variantDef.Name.ResolveGeneric(typeMap);
        } else {
            variantName = new IR.NamePath { Path = variantDef.Name.Path };
        }

        var name = this.CreateUniqueTypeName(variantName, out var ns);

        var typeDef = new TypeDefinition(ns,
            name,
            defAttrs,
            this.ValueType
        );

        this.assemblyBuilder.Module.Types.Add(typeDef);

        var typeID = this.cache.RegisterType(variantType, typeDef);

        var tagTypeRef = this.BuildType(variantDef.TagType);
        var tagField = new FieldDefinition(VariantTagName, FieldAttributes.Assembly, tagTypeRef) {
            Offset = 0,
        };

        // add the tag field attribute
        tagField.CustomAttributes.Add(new CustomAttribute(tagAttrCtor));

        typeDef.Fields.Add(tagField);

        var caseFieldRefs = new List<LayoutField?>(variantDef.Cases.Count);
        var caseTypeRefs = new (IR.IType defType, TypeReference typeRef)?[variantDef.Cases.Count];
        var caseTags = new object[variantDef.Cases.Count];

        for (var i = 0; i < variantDef.Cases.Count; i += 1) {
            var dataType = variantDef.Cases[i].Type;
            if (dataType is null or IR.NothingType) {
                continue;
            }

            caseTypeRefs[i] = (dataType, this.BuildType(dataType));

            var tagVal = variantDef.Cases[i].Tag;

            if (!tagVal.ToLiteralValue(out var tagLiteralVal) || tagLiteralVal == null) {
                var message = $"tag value {tagVal.ToString(metadata)} of {variantType.ToString(metadata)} is not a numeric literal";
                throw new InvalidDataException(message);
            }
            caseTags[i] = tagLiteralVal;
        }

        this.variantLayouts[typeID] = new VariantLayout {
            TagField = new LayoutField {
                Field = tagField,
                Type = variantDef.TagType,
            },
            Cases = caseFieldRefs,
        };

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

            var caseAttr = new CustomAttribute(caseAttrCtor);
            caseAttr.ConstructorArguments.Add(new CustomAttributeArgument(tagTypeRef, new CustomAttributeArgument(tagTypeRef, caseTags[i])));

            dataField.CustomAttributes.Add(caseAttr);

            typeDef.Fields.Add(dataField);
            caseFieldRefs.Add(new LayoutField {
                Field = dataField,
                Type = caseType,
            });
        }

        // discriminator (pointer sized) + any value members
        typeDef.ClassSize = this.GetVariantLayoutSize(variantDef);
        typeDef.PackingSize = 0;

        this.BuildInterfaceImpls(new IR.VariantType(variantRef), typeDef);

        this.BuildVariantRCMethods(typeID, variantDef, typeDef);

        this.BuildDefaultConstructor(typeDef);

        return typeDef;
    }

    private void BuildInterfaceImpls(IR.IType type, TypeDefinition typeDef) {
        if (!this.assemblyBuilder.LoadedMetadata.GetInterfaceImpls(type, out var typeImpls)) {
            return;
        }

        foreach (var ifaceRef in typeImpls.Keys) {
            var interfaceTypeRef = this.BuildType(ifaceRef.ToObjectID().ToObjectType());

            typeDef.Interfaces.Add(new InterfaceImplementation(interfaceTypeRef));
        }
    }

    private TypeReference BuildBoxTypeRef(IR.IType valueType) {
        var valueTypeRef = this.BuildType(valueType);

        // boxes just translate to 1-sized arrays
        var arrayType = valueTypeRef.MakeArrayType();
        this.RegisterSimpleType(valueType.MakeBox(), arrayType);

        return arrayType;
    }

    private FunctionPointerType BuildFunctionTypeDef(IR.FunctionSig sig) {
        var pointerType = new FunctionPointerType {
            ReturnType = this.BuildType(sig.ResultType),
        };

        foreach (var paramType in sig.ParameterTypes) {
            var parameterType = this.BuildType(paramType);
            pointerType.Parameters.Add(new ParameterDefinition(parameterType));
        }

        this.RegisterSimpleType(sig.ToFunctionType(), pointerType);

        return pointerType;
    }

    internal InterfaceTypeInstance GetInterfaceType(IR.InterfaceRef ifaceRef) {
        var ifaceType = ifaceRef.ToObjectID().ToObjectType();

        this.BuildType(ifaceType, out var typeID);

        return this.interfaceTypeCache[typeID];
    }

    private TypeReference BuildInterfaceDef(IR.InterfaceRef ifaceRef) {
        if (!this.assemblyBuilder.LoadedMetadata.FindInterfaceDecl(ifaceRef.DefID, out var ifaceDecl)
            || ifaceDecl is not IR.DefInterfaceDecl(var genericDef)
        ) {
            throw new InvalidDataException($"missing interface definition: {ifaceRef.DefID}");
        }

        IR.InterfaceDef ifaceDef;
        IR.NamePath defName;
        if (genericDef.Name.HasTypeParams) {
            var typeMap = IR.Util.BuildGenericTypeMap(genericDef.Name.TypeParams, ifaceRef.Args ?? []);
            ifaceDef = genericDef.ResolveGeneric(typeMap);
            defName = genericDef.Name.ResolveGeneric(typeMap);
        } else {
            ifaceDef = genericDef;
            defName = genericDef.Name.ToGenericName();
        }

        var name = this.CreateUniqueTypeName(defName, out var ns);

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

            var selfParam = ifaceMethod.Params.FirstOrDefault();

            var hasThis = selfParam != null && ifaceType.Equals(selfParam.Type);
            if (!hasThis) {
                methodAttrs |= MethodAttributes.Static;
            }

            var returnTypeRef = this.assemblyBuilder.TypeBuilder.BuildType(ifaceMethod.ResultType);
            var methodDef = new MethodDefinition(ifaceMethod.Name, methodAttrs, returnTypeRef) {
                HasThis = hasThis,
            };

            // if the method isn't static, the params list implicitly includes the self-type
            var restParams = !methodDef.IsStatic
                ? ifaceMethod.Params.Skip(1)
                : ifaceMethod.Params;

            foreach (var methodParam in restParams) {
                var paramTypeRef = this.BuildType(methodParam.Type);
                methodDef.Parameters.Add(new ParameterDefinition(paramTypeRef));
            }

            typeDef.Methods.Add(methodDef);

            this.interfaceMethods.Add((typeID, new IR.MethodID((ulong)methodIndex)), methodDef);
        }

        module.Types.Add(typeDef);

        var ifaceTypeInstance = new InterfaceTypeInstance {
            InterfaceDef = ifaceDef,
            TypeDef = typeDef,
        };
        this.interfaceTypeCache[typeID] = ifaceTypeInstance;

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

            var baseCtor = baseType
                .GetConstructors()
                .Single(ctor => !ctor.IsStatic && ctor.Parameters.Count == 0);

            var baseCtorRef = this.assemblyBuilder.Module.ImportReference(baseCtor);

            body.Emit(OpCodes.Ldarg_0);
            body.Emit(OpCodes.Call, baseCtorRef);
        }

        body.Emit(OpCodes.Ret);

        typeDef.Methods.Add(methodDef);
    }

    internal LayoutField GetFieldRef(IR.IType baseType, IR.FieldID fieldID) {
        // the closure field pointer can be accessed either through a closure object pointer
        // (accessing the pointer of an unknown closure type to call it) or directly as a member of a
        // specific closure class (setting the pointer during construction)
        if (fieldID.Equals(IR.FieldID.ClosurePointerField)) {
            if (baseType is IR.ObjectType(IR.AnyClosureObjectID(var sig))
                || (baseType is IR.StructType(var closureStructID)
                    && this.assemblyBuilder.LoadedMetadata.FindClosureSig(closureStructID.DefID, out sig))
            ) {
                return new LayoutField {
                    Field = this.closurePointerField,
                    Type = sig.ToFunctionType(),
                };
            }
        }

        this.BuildType(baseType, out var baseTypeID);

        if (!this.structLayouts.TryGetValue(baseTypeID, out var structFieldRefs)
            || !structFieldRefs.Fields.TryGetValue(fieldID, out var fieldRef)
        ) {
            var typeDisplay = baseType.ToString(this.assemblyBuilder.LoadedMetadata);

            throw new ArgumentException($"{typeDisplay} does not have field {fieldID.ID}");
        }

        return fieldRef;
    }

    public MethodReference GetStaticArrayElementMethodRef(IR.IType elementType, ulong dim) {
        this.BuildType(elementType, out var elementTypeID);

        var arraySig = new ArraySig(elementTypeID, dim);

        return this.staticArrayElementMethods[arraySig];
    }

    internal LayoutField GetVariantTagFieldRef(IR.IType baseType) {
        this.BuildType(baseType, out var baseTypeID);

        if (!this.variantLayouts.TryGetValue(baseTypeID, out var variantLayout)) {
            var typeDisplay = baseType.ToString(this.assemblyBuilder.LoadedMetadata);
            throw new ArgumentException($"variant {typeDisplay} is not defined yet");
        }

        return variantLayout.TagField;
    }

    internal LayoutField GetVariantDataFieldRef(IR.IType baseType, ulong caseIndex) {
        this.BuildType(baseType, out var baseTypeID);

        var typeDisplay = baseType.ToString(this.assemblyBuilder.LoadedMetadata);

        if (!this.variantLayouts.TryGetValue(baseTypeID, out var variantRefs)) {
            throw new ArgumentException($"variant ID {typeDisplay} is not defined yet");
        }

        return variantRefs.Cases[(int)caseIndex]
            ?? throw new ArgumentException($"invalid index {caseIndex} for variant type {typeDisplay}");
    }

    public GenericInstanceMethod GetObjectCreateMethod(IR.IObjectID classID) {
        var module = this.assemblyBuilder.Module;

        var classTypeRef = this.BuildType(new IR.ObjectType(classID));

        var methodInstance = new GenericInstanceMethod(module.ImportReference(this.ObjectCreateMethod));
        methodInstance.GenericArguments.Add(module.ImportReference(classTypeRef));

        return methodInstance;
    }

    public GenericInstanceMethod GetArrayCreateMethod(IR.IType elementType) {
        var module = this.assemblyBuilder.Module;
        
        var elementTypeRef = this.BuildType(elementType);

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
