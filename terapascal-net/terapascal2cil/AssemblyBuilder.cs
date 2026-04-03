using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using FieldAttributes = Mono.Cecil.FieldAttributes;
using MethodAttributes = Mono.Cecil.MethodAttributes;
using TypeAttributes = Mono.Cecil.TypeAttributes;

namespace Terapascal.CIL;

public class AssemblyBuilder : IDisposable {
    private readonly List<IR.Library> libraries;
    private TypeDefinition? globalsClass;
    private TypeDefinition? systemFuncsClass;

    private readonly MethodReference rttiAddTypeMethod;
    private readonly MethodReference rttiAddFuncMethod;

    private readonly Dictionary<IR.StringID, FieldDefinition> stringLitFields;
    private readonly Dictionary<IR.VariableID, FieldDefinition> globalVarFields;

    private readonly Dictionary<IR.StaticClosureID, FieldDefinition> staticClosureFields;

    private readonly Dictionary<IR.IType, FieldDefinition> staticTypeInfoFields;
    private readonly Dictionary<IR.FunctionID, FieldDefinition> staticFuncInfoFields;

    private readonly Dictionary<IR.ITagLocation, FieldDefinition> tagArrayFields;

    private readonly List<MethodDefinition> initMethods;

    private MethodDefinition? mainMethod;

    public AssemblyDefinition StandardLibrary { get; }
    public AssemblyDefinition RuntimeLibrary { get; }
    public AssemblyDefinition Assembly { get; }
    
    public TypeBuilder TypeBuilder { get; }
    public FunctionBuilder FunctionBuilder { get; }

    public ModuleDefinition Module => this.Assembly.MainModule;
    public TypeSystem TypeSystem => this.Assembly.MainModule.TypeSystem;

    public bool IsExecutable => this.Module.Kind is ModuleKind.Console or ModuleKind.Windows;

    public AssemblyBuilder(
        string assemblyName,
        Version assemblyVersion,
        ModuleKind moduleKind,
        string rtLibPath,
        string refLibPath
    ) {
        this.libraries = new List<IR.Library>(1);

        this.Assembly = AssemblyDefinition.CreateAssembly(
            new AssemblyNameDefinition(assemblyName, assemblyVersion),
            assemblyName,
            new ModuleParameters {
                Kind = moduleKind,
                Runtime = TargetRuntime.Net_4_0,
            });

        this.StandardLibrary = AssemblyDefinition.ReadAssembly(Path.Join(refLibPath, "netstandard.dll"));
        this.RuntimeLibrary = AssemblyDefinition.ReadAssembly(rtLibPath);

        this.TypeBuilder = new TypeBuilder(this);
        this.FunctionBuilder = new FunctionBuilder(this);

        this.stringLitFields = new Dictionary<IR.StringID, FieldDefinition>();
        this.globalVarFields = new Dictionary<IR.VariableID, FieldDefinition>();

        this.staticClosureFields = new Dictionary<IR.StaticClosureID, FieldDefinition>();

        this.staticTypeInfoFields = new Dictionary<IR.IType, FieldDefinition>();
        this.staticFuncInfoFields = new Dictionary<IR.FunctionID, FieldDefinition>();

        this.tagArrayFields = new Dictionary<IR.ITagLocation, FieldDefinition>(new IR.TagLocationComparer());

        this.initMethods = new List<MethodDefinition>(1);

        var rttiTypeDef = this.GetRuntimeTypeRef(nameof(Runtime.RTTI), false).Resolve();
        this.rttiAddFuncMethod = this.Module.ImportReference(rttiTypeDef.Methods
            .Single(m => m.Name == nameof(Runtime.RTTI.AddFunction)));
        this.rttiAddTypeMethod = this.Module.ImportReference(
            rttiTypeDef.Methods.Single(m => m.Name == nameof(Runtime.RTTI.AddType)));
    }

    public void Dispose() {
        this.Assembly.Dispose();
        
        this.RuntimeLibrary.Dispose();

        this.StandardLibrary.Dispose();
    }

    public TypeDefinition GetInternalClass() {
        if (this.globalsClass != null) {
            return this.globalsClass;
        }

        this.globalsClass = this.CreateClass(this.Assembly.Name.Name,$"{this.Assembly.Name.Name}_<Internal>");
        return this.globalsClass;
    }

    public TypeDefinition GetUnitClass(IR.NamePath unitPath) {
        var ns = unitPath.ToString();
        
        // we use the unit name as the namespace for type declared within it, so the unit class for free functions
        // etc must be differently named. we don't expect these to be accessible to other CLR code (since free
        // functions don't exist in CLR), so use an internal name
        const string name = "<Internal>";

        foreach (var typeDef in this.Module.Types) {
            if (typeDef.Namespace == ns && typeDef.Name == name) {
                return typeDef;
            }
        }

        return this.CreateClass(ns, name);
    }

    private TypeDefinition CreateClass(string ns, string name) {
        var objectType = this.Module.TypeSystem.Object;
        var newClass = new TypeDefinition(
            ns,
            name,
            TypeAttributes.Sealed | TypeAttributes.Class | TypeAttributes.Public,
            objectType
        );
        
        this.Module.Types.Add(newClass);

        return newClass;
    }

    private MethodDefinition GetMainMethod() {
        if (this.mainMethod != null) {
            return this.mainMethod;
        }
        
        var globalsClass = this.GetInternalClass();

        var attrs = MethodAttributes.Public | MethodAttributes.Static;
        var methodDef = new MethodDefinition("Main", attrs, this.TypeSystem.Void);

        globalsClass.Methods.Add(methodDef);

        this.mainMethod = methodDef;
        return methodDef;
    }

    private MethodDefinition GetGlobalsCCtor() {
        var globalsClass = this.GetInternalClass();

        var cctor = globalsClass.GetStaticConstructor();
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
            this.TypeSystem.Void
        );

        globalsClass.Methods.Add(methodDef);

        return methodDef;
    }
        
    public MethodReference FindRuntimeFunction(string name) {
        var systemFuncsName = typeof(Runtime.SystemFunctions).FullName;

        this.systemFuncsClass ??= this.RuntimeLibrary.MainModule.Types
            .SingleOrDefault(typeDef => typeDef.FullName == systemFuncsName)
            ?? throw new TypeLoadException($"runtime library DLL did not contain {systemFuncsName}");

        var methodDef = this.systemFuncsClass.Methods
            .SingleOrDefault(methodDef => methodDef.Name == name);

        if (methodDef == null) {
            var msg = $"missing method in runtime library: {name}";
            throw new ArgumentException(msg);
        }

        return this.Module.ImportReference(methodDef);
    }

    
    public TypeReference GetRuntimeTypeRef(string name, bool valueType) {
        var typeRef = new TypeReference("Terapascal.Runtime",
            name,
            this.RuntimeLibrary.MainModule,
            this.RuntimeLibrary.Name,
            valueType
        );

        return this.Module.ImportReference(typeRef.Resolve());
    }

    public FieldReference GetStringLiteralRef(IR.StringID id) {
        return this.stringLitFields[id];
    }
    
    public FieldReference GetGlobalVariableRef(IR.VariableID id) {
        return this.globalVarFields[id];
    }

    public void AddLibrary(IR.Library library) {
        this.libraries.Add(library);
        
        // TODO: native strings
        // we have to copy the string literals into pascal strings for now

        var globals = this.GetInternalClass();
        var stringTypeRef = this.GetRuntimeTypeRef(nameof(Runtime.String), false);

        var createStringMethod = this.FindRuntimeFunction(nameof(Runtime.SystemFunctions.CreateString));

        var globalsInit = this.GetGlobalsCCtor().Body.GetILProcessor();

        foreach (var (id, stringLit) in library.Metadata.StringLiterals) {
            var fieldAttrs = FieldAttributes.Assembly | FieldAttributes.InitOnly | FieldAttributes.Static;
            var fieldDef = new FieldDefinition($"String_{id.ID}", fieldAttrs, stringTypeRef);

            globals.Fields.Add(fieldDef);

            this.stringLitFields.Add(id, fieldDef);

            globalsInit.Emit(OpCodes.Ldstr, stringLit);
            globalsInit.Emit(OpCodes.Ldc_I4, 1);
            globalsInit.Emit(OpCodes.Call, createStringMethod);
            globalsInit.Emit(OpCodes.Stsfld, fieldDef);
        }

        const FieldAttributes globalVarFieldAttrs = FieldAttributes.Assembly | FieldAttributes.Static;

        foreach (var (id, varInfo) in library.Metadata.Variables) {
            var typeRef = this.TypeBuilder.BuildTypeRef(varInfo.Type, library);

            TypeDefinition typeDef;
            if (varInfo.Name.GetParent() is { } unitPath) {
                typeDef = this.GetUnitClass(unitPath);
            } else {
                typeDef = globals;
            }

            var varFieldDef = new FieldDefinition(varInfo.Name.Last, globalVarFieldAttrs, typeRef);
            typeDef.Fields.Add(varFieldDef);
            
            this.globalVarFields.Add(id, varFieldDef);
        }
        
        foreach (var (id, typeDecl) in library.Metadata.TypeDecls) {
            switch (typeDecl) {
                case IR.DefTypeDecl { Def: var def }: {
                    if (id == IR.TypeDefID.String 
                        || id == IR.TypeDefID.TypeInfo 
                        || id == IR.TypeDefID.MethodInfo 
                        || id == IR.TypeDefID.FunctionInfo) {
                        // skip classes defined in the runtime DLL
                        continue;
                    }
                    
                    switch (def) {
                        case IR.StructTypeDef { Def: var structDef }: {
                            this.TypeBuilder.BuildStructDef(id, structDef, library);
                            break;
                        }
                        case IR.VariantTypeDef { Def: var variantDef }: {
                            this.TypeBuilder.BuildVariantDef(id, variantDef, library);
                            break;
                        }
                        case IR.FunctionTypeDef { Sig: var sig }: {
                            this.TypeBuilder.BuildFunctionTypeDef(id, sig, library);
                            break;
                        }
                    }

                    break;
                }
            }
        }

        foreach (var staticClosure in library.StaticClosures) {
            var fieldAttrs = FieldAttributes.Assembly | FieldAttributes.Static;
            var fieldName = $"StaticClosure_{staticClosure.ID.ID}";
            var fieldDef = new FieldDefinition(fieldName, fieldAttrs, this.TypeBuilder.ClosureBaseType);
            
            globals.Fields.Add(fieldDef);
            
            this.staticClosureFields.Add(staticClosure.ID, fieldDef);
        }

        foreach (var (tagLoc, tags) in library.Metadata.GetAllTags()) {
            this.BuildStaticTagsArrayField(tagLoc, tags.Count, library);
        }

        foreach (var (type, typeInfo) in library.Metadata.TypeInfo) {
            this.BuildStaticTypeInfo(type, typeInfo, globals, library);
        }

        foreach (var (funcID, _) in library.Metadata.Functions) {
            this.CreateStaticFuncInfoVariable(funcID, library);
        }

        foreach (var (id, ifaceDecl) in library.Metadata.Interfaces) {
            if (ifaceDecl is IR.DefInterfaceDecl(var ifaceDef)) {
                this.TypeBuilder.BuildInterfaceDef(id, ifaceDef, library);
            }
        }

        this.FunctionBuilder.BuildFunctions(library);

        foreach (var (funcID, funcInfo) in library.Metadata.Functions) {
            this.BuildStaticFuncInfo(funcID, funcInfo, library);
        }

        foreach (var (type, typeInfo) in library.Metadata.TypeInfo) {
            this.BuildTypeInfoMethodsInit(type, typeInfo, library);
        }

        if (library.Initialization.Instructions.Count > 0) {
            var initAttrs = MethodAttributes.Private | MethodAttributes.Static;
            var initMethod = new MethodDefinition($"Init_{this.initMethods.Count}", initAttrs, this.TypeSystem.Void);

            this.initMethods.Add(initMethod);
            globals.Methods.Add(initMethod);

            // if this assembly is built as an executable, add each library's init call to the Main method,
            // otherwise add them to the cctor directory
            if (this.IsExecutable) {
                var mainMethod = this.GetMainMethod();

                mainMethod.Body.GetILProcessor().Emit(OpCodes.Call, initMethod);
            } else {
                globalsInit.Emit(OpCodes.Call, initMethod);
            }
            
            var initBuilder = new InstructionBuilder(this, library, initMethod);
            initBuilder.AddInstructions(library.Initialization.Instructions);
            initBuilder.Finish();
        }
    }

    private void BuildStaticTagsArrayField(IR.ITagLocation tagLoc, int count, IR.Library library) {
        var tagArrayTypeRef = this.TypeBuilder.BuildTypeRef(IR.IType.Any.MakeDynArray(), library);
        var arrayCreateInstance = this.TypeBuilder.GetArrayCreateMethod(IR.IType.Any, library);

        if (count == 0) {
            return;
        }

        const FieldAttributes tagFieldAttrs = FieldAttributes.Static | FieldAttributes.Assembly;
        var tagFieldDef = new FieldDefinition(tagLoc.GetUniqueName(), tagFieldAttrs, tagArrayTypeRef);
        this.globalsClass!.Fields.Add(tagFieldDef);

        var initBody = this.GetGlobalsCCtor().Body.GetILProcessor();
        initBody.Emit(OpCodes.Ldc_I4, count);
        initBody.Emit(OpCodes.Ldc_I4_1); // immortal
        initBody.Emit(OpCodes.Call, arrayCreateInstance);
        initBody.Emit(OpCodes.Stsfld, tagFieldDef);
            
        this.tagArrayFields.Add(tagLoc, tagFieldDef);
    }

    private void BuildStaticTypeInfo(
        IR.IType type,
        IR.TypeInfo typeInfo,
        TypeDefinition globals,
        IR.Library library
    ) {
        var typeRef = this.TypeBuilder.BuildTypeRef(type, library);

        var typeInfoClassID = new IR.ClassObjectID(IR.TypeDefID.TypeInfo);
        var typeInfoType = IR.TypeDefID.TypeInfo.ToObjectType();

        var nameField = this.TypeBuilder.GetFieldRef(typeInfoType, IR.FieldID.TypeInfoName, library);
        var implField = this.TypeBuilder.GetFieldRef(typeInfoType, IR.FieldID.TypeInfoImpl, library);
        var tagsField = this.TypeBuilder.GetFieldRef(typeInfoType, IR.FieldID.TypeInfoTags, library);
        var flagsField = this.TypeBuilder.GetFieldRef(typeInfoType, IR.FieldID.TypeInfoFlags, library);

        var typeInfoTypeRef = this.Module
            .ImportReference(this.TypeBuilder
            .BuildTypeRef(typeInfoType, library));

        var fieldName = $"StaticTypeInfo_{type.GetUniqueName()}";

        const FieldAttributes fieldAttrs = FieldAttributes.Assembly | FieldAttributes.Static;
        var fieldDef = new FieldDefinition(fieldName, fieldAttrs, typeInfoTypeRef);

        globals.Fields.Add(fieldDef);
        this.staticTypeInfoFields.Add(type, fieldDef);

        var initBuilder = this.GetGlobalsCCtor();
        var initBody = initBuilder.Body.GetILProcessor();

        var createTypeInfoInst = this.TypeBuilder.GetObjectCreateMethod(typeInfoClassID, library);
        
        initBody.Emit(OpCodes.Ldc_I4_1); // immortal: true
        initBody.Emit(OpCodes.Call, createTypeInfoInst);
        initBody.Emit(OpCodes.Stsfld, fieldDef);

        // register in RTTI list
        initBody.Emit(OpCodes.Ldsfld, fieldDef);
        initBody.Emit(OpCodes.Call, this.rttiAddTypeMethod);

        var nameStringFieldRef = this.GetStringLiteralRef(typeInfo.Name ?? IR.StringID.EmptyString);
        initBody.Emit(OpCodes.Ldsfld, fieldDef);
        initBody.Emit(OpCodes.Ldsfld, nameStringFieldRef);
        initBody.Emit(OpCodes.Stfld, nameField);

        // set type pointer
        initBody.Emit(OpCodes.Ldsfld, fieldDef);
        initBody.Emit(OpCodes.Ldtoken, typeRef);
        initBody.Emit(OpCodes.Call, this.TypeBuilder.GetTypeFromHandleMethod);
        initBody.Emit(OpCodes.Stfld, implField);
        
        // set tags array
        var typeTagsLoc = type.GetTagsLocation();
        if (typeTagsLoc != null && this.GetStaticTagArrayFieldRef(typeTagsLoc) is {} tagsArrayFieldRef) {
            initBody.Emit(OpCodes.Ldsfld, fieldDef);
            initBody.Emit(OpCodes.Ldsfld, tagsArrayFieldRef);
            initBody.Emit(OpCodes.Stfld, tagsField);
        }
        
        // set flags
        initBody.Emit(OpCodes.Ldsfld, fieldDef);
        unchecked {
            initBody.Emit(OpCodes.Ldc_I8, (long)typeInfo.Flags);
        }
        initBody.Emit(OpCodes.Stfld, flagsField);
    }

    private void BuildTypeInfoMethodsInit(
        IR.IType type,
        IR.TypeInfo typeInfo,
        IR.Library library
    ) {
        var typeInfoFieldRef = this.GetStaticTypeInfoFieldRef(type);
        if (typeInfoFieldRef == null) {
            var msg = $"missing typeinfo field: typeinfo was not built yet for {type.ToPrettyString(library.Metadata)}";
            throw new InvalidDataException(msg);
        }

        var typeInfoType = IR.TypeDefID.TypeInfo.ToObjectType();
        var methodsField = this.TypeBuilder.GetFieldRef(typeInfoType, IR.FieldID.TypeInfoMethods, library);

        var initBuilder = this.GetGlobalsCCtor();
        var initBody = initBuilder.Body.GetILProcessor();
        
        var methodInfoClassID = new IR.ClassObjectID(IR.TypeDefID.MethodInfo);
        var methodInfoTypeRef = this.TypeBuilder.BuildTypeRef(IR.TypeDefID.MethodInfo.ToObjectType(), library);
        var methodInfoTypeDef = methodInfoTypeRef.Resolve();
            
        var createMethodInfoInst = this.TypeBuilder.GetObjectCreateMethod(methodInfoClassID, library);
        var methodNameField =  this.Module.ImportReference(methodInfoTypeDef.GetFieldByName(nameof(Runtime.MethodInfo.name)));
        var methodImplField =  this.Module.ImportReference(methodInfoTypeDef.GetFieldByName(nameof(Runtime.MethodInfo.impl)));
        var methodOwnerField = this.Module.ImportReference(methodInfoTypeDef.GetFieldByName(nameof(Runtime.MethodInfo.owner)));
        var methodTagsField = this.Module.ImportReference(methodInfoTypeDef.GetFieldByName(nameof(Runtime.MethodInfo.tags)));

        initBody.Emit(OpCodes.Ldsfld, typeInfoFieldRef);

        initBody.Emit(OpCodes.Ldc_I4, typeInfo.Methods.Count);
        initBody.Emit(OpCodes.Newarr, methodInfoTypeRef);

        for (var i = 0; i < typeInfo.Methods.Count; i++) {
            var method = typeInfo.Methods[i];
            initBody.Emit(OpCodes.Dup);
            initBody.Emit(OpCodes.Ldc_I4, i);

            initBody.Emit(OpCodes.Ldc_I4_1); // immortal: true
            initBody.Emit(OpCodes.Call, createMethodInfoInst);

            // set name field
            initBody.Emit(OpCodes.Dup);
            initBody.Emit(OpCodes.Ldsfld, this.GetStringLiteralRef(method.Name));
            initBody.Emit(OpCodes.Stfld, methodNameField);

            // set impl field (null for abstract methods)
            if (method.Function != null
                && library.Metadata.Functions.TryGetValue(method.Function.Value, out var methodFuncInfo)
                && methodFuncInfo.Invoker.HasValue
            ) {
                initBody.Emit(OpCodes.Dup);
                this.BuildFunctionInfoImplObject(initBody, method.Function.Value, methodFuncInfo.Invoker);
                initBody.Emit(OpCodes.Stfld, methodImplField);
            }

            // set owner field
            initBody.Emit(OpCodes.Dup);
            initBody.Emit(OpCodes.Ldsfld, typeInfoFieldRef);
            initBody.Emit(OpCodes.Stfld, methodOwnerField);

            var methodTagsLoc = type.GetTagsLocation()?.MethodLocation(method.Index);
            if (methodTagsLoc != null && method.Tags.Count > 0) {
                initBody.Emit(OpCodes.Dup);
                initBody.Emit(OpCodes.Ldsfld, this.GetStaticTagArrayFieldRef(methodTagsLoc));
                initBody.Emit(OpCodes.Stfld, methodTagsField);
            }

            initBody.Emit(OpCodes.Stelem_Any, methodInfoTypeRef);
        }

        initBody.Emit(OpCodes.Stfld, methodsField);
    }

    private void CreateStaticFuncInfoVariable(IR.FunctionID funcID, IR.Library library) {
        var funcInfoType = IR.TypeDefID.FunctionInfo.ToObjectType();
        var funcInfoTypeRef = this.TypeBuilder.BuildTypeRef(funcInfoType, library);

        var fieldName = $"StaticFuncInfo_{funcID.ID}";

        const FieldAttributes fieldAttrs = FieldAttributes.Assembly | FieldAttributes.Static;
        var fieldDef = new FieldDefinition(fieldName, fieldAttrs, funcInfoTypeRef);

        this.GetInternalClass().Fields.Add(fieldDef);
        this.staticFuncInfoFields.Add(funcID, fieldDef);
    }

    private void BuildStaticFuncInfo(IR.FunctionID funcID, IR.FunctionInfo funcInfo, IR.Library library) {
        var fieldRef = this.GetStaticFuncInfoFieldRef(funcID)!;

        var funcInfoType = IR.TypeDefID.FunctionInfo.ToObjectType();
        var funcObjectID = new IR.ClassObjectID(IR.TypeDefID.FunctionInfo);

        var nameField = this.TypeBuilder.GetFieldRef(funcInfoType, IR.FieldID.FunctionInfoName, library);
        var implField = this.TypeBuilder.GetFieldRef(funcInfoType, IR.FieldID.FunctionInfoImpl, library);
        var tagsField = this.TypeBuilder.GetFieldRef(funcInfoType, IR.FieldID.FunctionInfoTags, library);

        var initBuilder = this.GetGlobalsCCtor();
        var initBody = initBuilder.Body.GetILProcessor();

        var createTypeInfoInst = this.TypeBuilder.GetObjectCreateMethod(funcObjectID, library);
        
        initBody.Emit(OpCodes.Ldc_I4_1); // immortal: true
        initBody.Emit(OpCodes.Call, createTypeInfoInst);

        var nameStringFieldRef = this.GetStringLiteralRef(funcInfo.RuntimeName ?? IR.StringID.EmptyString);
        initBody.Emit(OpCodes.Dup);
        initBody.Emit(OpCodes.Ldsfld, nameStringFieldRef);
        initBody.Emit(OpCodes.Stfld, nameField);

        // set impl pointer
        initBody.Emit(OpCodes.Dup);
        this.BuildFunctionInfoImplObject(initBody, funcID, funcInfo.Invoker);
        initBody.Emit(OpCodes.Stfld, implField);

        // set tags array
        var funcTagsLocation = new IR.FunctionTagLocation(funcID);
        if (this.GetStaticTagArrayFieldRef(funcTagsLocation) is {} tagsArrayFieldRef) {
            initBody.Emit(OpCodes.Dup);
            initBody.Emit(OpCodes.Ldsfld, tagsArrayFieldRef);
            initBody.Emit(OpCodes.Stfld, tagsField);
        }
        
        // register in RTTI list
        initBody.Emit(OpCodes.Dup);
        initBody.Emit(OpCodes.Call, this.rttiAddFuncMethod);

        initBody.Emit(OpCodes.Stsfld, fieldRef);
    }

    private void BuildFunctionInfoImplObject(ILProcessor body, IR.FunctionID funcID, IR.FunctionID? invokerID) {
        var implTypeDef = this.GetRuntimeTypeRef(nameof(Runtime.FunctionInfoImpl), false).Resolve();
        var implTypeCtor = this.Module.ImportReference(implTypeDef.FindConstructor([]));
        
        var implMethodField = this.Module.ImportReference(implTypeDef.GetFieldByName(nameof(Runtime.FunctionInfoImpl.method))!);
        var implInvokerField = this.Module.ImportReference(implTypeDef.GetFieldByName(nameof(Runtime.FunctionInfoImpl.invoker))!);

        var methodRef = this.FunctionBuilder.FindFunctionMethod(funcID)
            ?? throw new InvalidDataException($"function {funcID.ID} was not defined");

        body.Emit(OpCodes.Newobj, implTypeCtor);

        body.Emit(OpCodes.Dup);
        body.Emit(OpCodes.Ldtoken, methodRef);
        body.Emit(OpCodes.Call, this.TypeBuilder.GetMethodFromHandleMethod);
        body.Emit(OpCodes.Stfld, implMethodField);

        if (invokerID != null) {
            var invokerRef = this.FunctionBuilder.FindFunctionMethod(invokerID.Value)!;
            body.Emit(OpCodes.Dup);
            body.Emit(OpCodes.Ldftn, invokerRef);
            body.Emit(OpCodes.Stfld, implInvokerField);
        }
    }

    public IR.FunctionSig? GetFunctionPointerType(IR.TypeDefID id) {
        foreach (var lib in this.libraries) {
            if (!lib.Metadata.TypeDecls.TryGetValue(id, out var decl)) {
                continue;
            }

            if (decl is IR.DefTypeDecl(IR.FunctionTypeDef(var sig))) {
                return sig;
            } 
        }

        return null;
    }

    public FieldReference GetStaticClosureFieldRef(IR.StaticClosureID id) {
        return this.staticClosureFields[id];
    }
    
    public FieldReference? GetStaticTypeInfoFieldRef(IR.IType type) {
        return this.staticTypeInfoFields.GetValueOrDefault(type);
    }
    
    public FieldReference? GetStaticFuncInfoFieldRef(IR.FunctionID funcID) {
        return this.staticFuncInfoFields.GetValueOrDefault(funcID);
    }

    public FieldReference? GetStaticTagArrayFieldRef(IR.ITagLocation tagLocation) {
        return this.tagArrayFields.GetValueOrDefault(tagLocation);
    }

    public bool IsClosureStruct(IR.TypeDefID closureStructID) {
        return this.libraries.Any(lib => lib.Metadata.Closures
            .Any(funcClosures => funcClosures.Value.Contains(closureStructID)));
    }

    public void Finish() {
        var initFunction = this.GetGlobalsCCtor();
        var globalsInit = initFunction.Body.GetILProcessor();

        globalsInit.Emit(OpCodes.Ret);

        if (this.IsExecutable) {
            this.GetMainMethod().Body.GetILProcessor().Emit(OpCodes.Ret);
            this.Assembly.EntryPoint = this.GetMainMethod();
        }
    }
}
