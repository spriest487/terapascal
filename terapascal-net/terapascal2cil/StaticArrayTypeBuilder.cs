using System.Runtime.CompilerServices;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using FieldAttributes = Mono.Cecil.FieldAttributes;
using MethodAttributes = Mono.Cecil.MethodAttributes;
using ParameterAttributes = Mono.Cecil.ParameterAttributes;
using PropertyAttributes = Mono.Cecil.PropertyAttributes;
using TypeAttributes = Mono.Cecil.TypeAttributes;

namespace Terapascal.CIL;

public class StaticArrayTypeBuilder {
    private const string ElementsFieldName = "elements";
    private const string IndexPropName = "Item";

    private readonly AssemblyBuilder assemblyBuilder;
    private readonly TypeBuilder typeBuilder;

    public StaticArrayTypeBuilder(AssemblyBuilder assemblyBuilder, TypeBuilder typeBuilder) {
        this.assemblyBuilder = assemblyBuilder;
        this.typeBuilder = typeBuilder;
    }

    public TypeReference BuildArrayTypeRef(IR.IType element, int id, int length, out MethodDefinition elementRefMethodDef) {
        var typeName = $"StaticArray_Internal{id}";

        var elementTypeRef = this.typeBuilder.BuildType(element);

        var systemTypesNamespace = typeof(Runtime.SystemFunctions).Namespace;
    
        var typeAttributes = TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.SequentialLayout | TypeAttributes.AnsiClass | TypeAttributes.BeforeFieldInit;
        var typeDef = new TypeDefinition(systemTypesNamespace, typeName, typeAttributes, this.typeBuilder.ValueType);

        // value types can be represented as a fixed buffer (C#'s fixed array syntax).
        // reference type elements must be represented as sequential fields with runtime code to look up by index
        switch (elementTypeRef.Namespace, elementTypeRef.Name) {
            case (nameof(System), nameof(SByte)): {
                this.BuildPrimitiveFixedArray(typeDef, elementTypeRef, element, length, OpCodes.Ldind_I1, OpCodes.Stind_I1, out elementRefMethodDef);
                break;
            }
            case (nameof(System), nameof(Byte)): {
                this.BuildPrimitiveFixedArray(typeDef, elementTypeRef, element, length, OpCodes.Ldind_U1, OpCodes.Stind_I1, out elementRefMethodDef);
                break;
            }
            case (nameof(System), nameof(Int16)): {
                this.BuildPrimitiveFixedArray(typeDef, elementTypeRef, element, length, OpCodes.Ldind_I2, OpCodes.Stind_I2, out elementRefMethodDef);
                break;
            }
            case (nameof(System), nameof(UInt16)): {
                this.BuildPrimitiveFixedArray(typeDef, elementTypeRef, element, length, OpCodes.Ldind_U2, OpCodes.Stind_I2, out elementRefMethodDef);
                break;
            }
            case (nameof(System), nameof(Int32)): {
                this.BuildPrimitiveFixedArray(typeDef, elementTypeRef, element, length, OpCodes.Ldind_I4, OpCodes.Stind_I4, out elementRefMethodDef);
                break;
            }
            case (nameof(System), nameof(UInt32)): {
                this.BuildPrimitiveFixedArray(typeDef, elementTypeRef, element, length, OpCodes.Ldind_U4, OpCodes.Stind_I4, out elementRefMethodDef);
                break;
            }
            case (nameof(System), nameof(Single)): {
                this.BuildPrimitiveFixedArray(typeDef, elementTypeRef, element, length, OpCodes.Ldind_R4, OpCodes.Stind_R4, out elementRefMethodDef);
                break;
            }
            case (nameof(System), nameof(Double)): {
                this.BuildPrimitiveFixedArray(typeDef, elementTypeRef, element, length, OpCodes.Ldind_R8, OpCodes.Stind_R8, out elementRefMethodDef);
                break;
            }
            case (nameof(System), nameof(Int64)):
            case (nameof(System), nameof(UInt64)): {
                this.BuildPrimitiveFixedArray(typeDef, elementTypeRef, element, length, OpCodes.Ldind_I8, OpCodes.Stind_I8, out elementRefMethodDef);
                break;
            }
            case (nameof(System), nameof(IntPtr)):
            case (nameof(System), nameof(UIntPtr)): {
                this.BuildPrimitiveFixedArray(typeDef, elementTypeRef, element, length, OpCodes.Ldind_I, OpCodes.Stind_I, out elementRefMethodDef);
                break;
            }

            default: {
                for (var i = 0; i < length; i += 1) {
                    var elementField = new FieldDefinition($"element{i}", FieldAttributes.Public, elementTypeRef);
                    typeDef.Fields.Add(elementField);
                }
            
                var indexer = this.BuildIndexerProperty(typeDef, elementTypeRef);
                this.BuildElementIndexerMethods(indexer);

                elementRefMethodDef = this.BuildElementReferenceFunction(typeDef, elementTypeRef);
                this.BuildElementReferenceMethodBody(elementRefMethodDef, typeDef);

                break;
            }
        }
        
        this.typeBuilder.BuildDefaultConstructor(typeDef);
        
        typeDef.Methods.Add(elementRefMethodDef);

        this.assemblyBuilder.Module.Types.Add(typeDef);

        return typeDef;
    }

    private void BuildPrimitiveFixedArray(
        TypeDefinition typeDef,
        TypeReference elementTypeRef,
        IR.IType element,
        int length,
        OpCode loadIndOp,
        OpCode storeIndOp,
        out MethodDefinition elementRefMethodDef
    ) {
        var elementSize = this.typeBuilder.GetValueLayoutSize(element);

        var bufferElementTypeDef = this.BuildArrayFixedBufferType(ElementsFieldName, element, length, elementSize);
        typeDef.NestedTypes.Add(bufferElementTypeDef);

        var elementField = new FieldDefinition(ElementsFieldName, FieldAttributes.Public, bufferElementTypeDef);

        var fixedBufferAttrCtor = this.FindCoreAttributeConstructor(
            typeof(FixedBufferAttribute),
            typeof(Type),
            typeof(int)
        );

        var fixedBufferAttr = new CustomAttribute(this.typeBuilder.ImportCoreReference(fixedBufferAttrCtor));
        fixedBufferAttr.ConstructorArguments.Add(new CustomAttributeArgument(this.typeBuilder.TypeType, elementTypeRef));
        fixedBufferAttr.ConstructorArguments.Add(new CustomAttributeArgument(this.assemblyBuilder.TypeSystem.Int32, length));
        
        elementField.CustomAttributes.Add(fixedBufferAttr);
        typeDef.Fields.Add(elementField);

        typeDef.PackingSize = 0;
        typeDef.ClassSize = bufferElementTypeDef.ClassSize;

        var indexer = this.BuildIndexerProperty(typeDef, elementTypeRef);
        this.BuildPrimitiveElementIndexer(indexer, bufferElementTypeDef, elementSize, loadIndOp, storeIndOp);

        elementRefMethodDef = this.BuildElementReferenceFunction(typeDef, elementTypeRef);
        this.BuildPrimitiveElementReferenceMethodBody(
            elementRefMethodDef,
            typeDef,
            bufferElementTypeDef,
            elementSize
        );
    }

    private MethodReference FindCoreAttributeConstructor(Type attrType, params Type[]? paramTypes) {
        var typeRef = this.typeBuilder.ImportCoreReference(attrType.Namespace, attrType.Name, false);
        var typeDef = this.typeBuilder.ResolveCore(typeRef)!;
        var ctorRef = TypeBuilder.FindCustomAttributeConstructor(typeDef, paramTypes);

        return this.typeBuilder.ImportCoreReference(ctorRef);
    }

    private TypeDefinition BuildArrayFixedBufferType(
        string fieldName,
        IR.IType elementType,
        int length,
        int elementSize
    ) {
        var attrs = TypeAttributes.NestedPublic 
            | TypeAttributes.Sealed 
            | TypeAttributes.SequentialLayout 
            | TypeAttributes.AnsiClass 
            | TypeAttributes.BeforeFieldInit;

        var typeDef = new TypeDefinition(null, $"<{fieldName}>e__FixedBuffer", attrs, this.typeBuilder.ValueType) {
            ClassSize = elementSize * length,
            PackingSize = 0,
        };

        var unsafeValAttrCtor = this.FindCoreAttributeConstructor(typeof(UnsafeValueTypeAttribute));
        var compilerGenCtor = this.FindCoreAttributeConstructor(typeof(CompilerGeneratedAttribute));

        typeDef.CustomAttributes.Add(new CustomAttribute(unsafeValAttrCtor));
        typeDef.CustomAttributes.Add(new CustomAttribute(compilerGenCtor));

        var fieldTypeRef = this.typeBuilder.BuildType(elementType);

        var fieldAttrs = FieldAttributes.Public;
        typeDef.Fields.Add(new FieldDefinition("FixedElementField", fieldAttrs, fieldTypeRef));

        return typeDef;
    }

    private PropertyDefinition BuildIndexerProperty(TypeDefinition typeDef, TypeReference elementTypeRef) {
        const string indexParamName = "index";
        const string valueParamName = "value";

        const MethodAttributes methodAttrs = MethodAttributes.Public 
            | MethodAttributes.SpecialName 
            | MethodAttributes.HideBySig;
        
        var indexType = this.assemblyBuilder.TypeSystem.Int32;

        var defaultMemberAttrCtor = this.FindCoreAttributeConstructor(typeof(System.Reflection.DefaultMemberAttribute), typeof(string));
        var defaultMemberAttr = new CustomAttribute(defaultMemberAttrCtor);
        defaultMemberAttr.ConstructorArguments.Add(new CustomAttributeArgument(this.typeBuilder.CLRStringType, IndexPropName));

        typeDef.CustomAttributes.Add(defaultMemberAttr);

        var indexPropDef = new PropertyDefinition(IndexPropName, PropertyAttributes.None, elementTypeRef);
        
        // getter
        var getMethod = new MethodDefinition($"get_{IndexPropName}", methodAttrs, elementTypeRef);
        getMethod.Parameters.Add(new ParameterDefinition(indexParamName, ParameterAttributes.None, indexType));
        getMethod.Body = new MethodBody(getMethod);

        // setter
        var setMethod = new MethodDefinition($"set_{IndexPropName}", methodAttrs, this.assemblyBuilder.TypeSystem.Void);
        setMethod.Parameters.Add(new ParameterDefinition(valueParamName, ParameterAttributes.None, elementTypeRef));
        setMethod.Parameters.Add(new ParameterDefinition(indexParamName, ParameterAttributes.None, indexType));
        setMethod.Body = new MethodBody(setMethod);

        indexPropDef.GetMethod = getMethod;
        indexPropDef.SetMethod = setMethod;

        typeDef.Properties.Add(indexPropDef);
        typeDef.Methods.Add(getMethod);
        typeDef.Methods.Add(setMethod);

        return indexPropDef;
    }

    private void BuildPrimitiveElementIndexer(
        PropertyDefinition indexPropDef,
        TypeDefinition bufferTypeDef,
        int elementSize,
        OpCode loadIndOp,
        OpCode storeIndOp
    ) {
        var typeDef = indexPropDef.DeclaringType;

        var bufferFieldRef = typeDef.Fields[0];
        var elementFieldRef = bufferTypeDef.Fields[0];
        
        var getBody = indexPropDef.GetMethod.Body.GetILProcessor();
        getBody.Emit(OpCodes.Ldarg_0);
        getBody.Emit(OpCodes.Ldflda, bufferFieldRef);
        getBody.Emit(OpCodes.Ldflda, elementFieldRef);
        
        this.BuildValueElementOffset(getBody, elementSize, OpCodes.Ldarg_1);
       
        getBody.Emit(loadIndOp);
        getBody.Emit(OpCodes.Ret);
        
        var setBody = indexPropDef.SetMethod.Body.GetILProcessor();
        setBody.Emit(OpCodes.Ldarg_0);
        setBody.Emit(OpCodes.Ldflda, bufferFieldRef);
        setBody.Emit(OpCodes.Ldflda, elementFieldRef);
        
        this.BuildValueElementOffset(setBody, elementSize, OpCodes.Ldarg_2);
        
        setBody.Emit(OpCodes.Ldarg_1);

        setBody.Emit(storeIndOp);
        setBody.Emit(OpCodes.Ret);
    }

    private void BuildValueElementOffset(ILProcessor body, int elementSize, OpCode loadIndexArg) {
        // load index
        body.Emit(loadIndexArg);
        body.Emit(OpCodes.Conv_I);
        
        // load element size
        body.Emit(OpCodes.Ldc_I4, elementSize);
        
        // multiply element size by offset to get total offset
        body.Emit(OpCodes.Mul);
        
        // offset pointer by total offset
        body.Emit(OpCodes.Add);
    }

    private void BuildElementIndexerMethods(PropertyDefinition indexPropDef) {
        var typeDef = indexPropDef.DeclaringType;

        // jump table for loading each field
        var loadFieldInstructions = new Instruction[typeDef.Fields.Count];
        var storeFieldInstructions = new Instruction[typeDef.Fields.Count];
        for (var i = 0; i < loadFieldInstructions.Length; i += 1) {
            var fieldRef = typeDef.Fields[i];

            loadFieldInstructions[i] = Instruction.Create(OpCodes.Ldfld, fieldRef);
            storeFieldInstructions[i] = Instruction.Create(OpCodes.Stfld, fieldRef);
        }

        {
            var retInstruction = Instruction.Create(OpCodes.Ret);

            var getBody = indexPropDef.GetMethod.Body.GetILProcessor();
            getBody.Emit(OpCodes.Ldarg_0);

            // switch (index)...
            getBody.Emit(OpCodes.Ldarg_1);
            
            // this falls through to the first one if it doesn't match, but the source language should already
            // do bounds checking when this is used for Element instructions
            getBody.Emit(OpCodes.Switch, loadFieldInstructions);
            foreach (var fieldInstruction in loadFieldInstructions) {
                getBody.Append(fieldInstruction);
                getBody.Emit(OpCodes.Br, retInstruction);
            }

            getBody.Append(retInstruction);
        }

        {
            var retInstruction = Instruction.Create(OpCodes.Ret);

            var setBody = indexPropDef.SetMethod.Body.GetILProcessor();
            setBody.Emit(OpCodes.Ldarg_0);
            setBody.Emit(OpCodes.Ldarg_1);

            // switch (index)...
            setBody.Emit(OpCodes.Ldarg_2);
            
            // this falls through to the first one if it doesn't match, but the source language should already
            // do bounds checking when this is used for Element instructions
            setBody.Emit(OpCodes.Switch, storeFieldInstructions);
            foreach (var fieldInstruction in storeFieldInstructions) {
                setBody.Append(fieldInstruction);
                setBody.Emit(OpCodes.Br, retInstruction);
            }
            
            setBody.Append(retInstruction);
        }
    }

    private MethodDefinition BuildElementReferenceFunction(TypeReference arrayTypeRef, TypeReference elementTypeRef) {
        var refTypeRef = elementTypeRef.MakeByReferenceType();
        
        var methodDef = new MethodDefinition("RefElement", MethodAttributes.Assembly | MethodAttributes.Static, refTypeRef);
        methodDef.Parameters.Add(new ParameterDefinition(arrayTypeRef.MakeByReferenceType()));
        methodDef.Parameters.Add(new ParameterDefinition(this.assemblyBuilder.TypeSystem.Int32));

        methodDef.Body = new MethodBody(methodDef);
        
        return methodDef;
    }

    private void BuildPrimitiveElementReferenceMethodBody(
        MethodDefinition methodDef,
        TypeDefinition arrayTypeDef,
        TypeDefinition bufferTypeDef,
        int elementSize
    ) {
        var body = methodDef.Body.GetILProcessor();

        var bufferFieldRef = arrayTypeDef.Fields[0];
        var elementFieldRef = bufferTypeDef.Fields[0];
        
        body.Emit(OpCodes.Ldarg_0);
        body.Emit(OpCodes.Ldflda, bufferFieldRef);
        body.Emit(OpCodes.Ldflda, elementFieldRef);
        
        this.BuildValueElementOffset(body, elementSize, OpCodes.Ldarg_1);

        body.Emit(OpCodes.Ret);
    }

    private void BuildElementReferenceMethodBody(MethodDefinition methodDef, TypeDefinition arrayTypeDef) {
        var body = methodDef.Body.GetILProcessor();

        var retInstruction = body.Create(OpCodes.Ret);

        body.Emit(OpCodes.Ldarg_0);

        var loadFieldAddrInstructions = new Instruction[arrayTypeDef.Fields.Count];
        for (var i = 0; i < loadFieldAddrInstructions.Length; i += 1) {
            loadFieldAddrInstructions[i] = Instruction.Create(OpCodes.Ldflda, arrayTypeDef.Fields[i]);
        }

        body.Emit(OpCodes.Ldarg_1);
        
        // this falls through to the first one if it doesn't match, but the source language should already
        // do bounds checking when this is used for Element instructions
        body.Emit(OpCodes.Switch, loadFieldAddrInstructions);
        foreach (var loadAddrInstruction in loadFieldAddrInstructions) {
            body.Append(loadAddrInstruction);
            body.Emit(OpCodes.Br, retInstruction);
        }

        body.Append(retInstruction);
    }
}
