mod builder;

use crate::c::boxed::BoxTypeID;
use crate::c::function::builder::FuncInstanceBuilder;
use crate::c::type_map::TypeID;
use crate::c::CBuilder;
use crate::c::DynArrayTypeID;
use crate::c::Expr;
use crate::c::Statement;
use crate::c::Type;
use crate::c::TypeDecl;
use crate::c::TypeDefName;
use crate::c::Unit;
use crate::c::VariableID;
use std::borrow::Cow;
use std::env;
use std::fmt;
use std::rc::Rc;
use terapascal_ir as ir;
use ir::generic::build_invocation_type_map;
use ir::generic::instantiate_function_def;
use ir::generic::instantiate_sig;
use ir::MetadataSource;

#[derive(Copy, Clone)]
pub struct FunctionInstance {
    pub id: FuncInstanceID,
    pub name: FunctionName,
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd)]
pub struct FuncInstanceID(pub usize);

impl fmt::Display for FuncInstanceID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum FunctionName {
    // c main function
    Main,

    // init function that all loaded modules append their init code into
    Init,

    // function to initialize an external symbol at runtime
    LoadSymbol,

    ID(FuncInstanceID),
    Extern(ir::FunctionID),
    Method(TypeID, ir::MethodID),
    Destructor(TypeDefName),
    VirtualMethodWrapper(TypeID, ir::MethodID, TypeDefName),

    // generated dynarray methods
    DynArrayGetElement(DynArrayTypeID),
    DynArrayAlloc(DynArrayTypeID),
    DynArrayLength(DynArrayTypeID),
    
    Forget,
    
    BoxValue(BoxTypeID),
    
    // helper function for runtime bounds checking
    DynArrayBoundsCheck,

    Builtin(BuiltinName),
}

impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionName::Main => write!(f, "main"),
            FunctionName::Init => write!(f, "ModuleInit"),
            FunctionName::LoadSymbol => write!(f, "LoadSymbol"),

            FunctionName::ID(id) => write!(f, "Function_{}", id.0),
            FunctionName::Extern(id) => write!(f, "Extern{}", id.0),

            FunctionName::Destructor(name) => write!(f, "Destructor_{}", name),

            FunctionName::Method(iface, method) => {
                write!(f, "Method_{}_{}", iface, method.0)
            },
            FunctionName::VirtualMethodWrapper(iface, method, self_ty) => {
                write!(f, "Method_{}_{}_Wrapper_{}", iface, method.0, self_ty)
            },

            FunctionName::DynArrayGetElement(id) => {
                write!(f, "DynArrayGetElement_{}", id.0)
            }
            FunctionName::DynArrayLength(id) => {
                write!(f, "DynArrayLength_{}", id.0)
            }
            FunctionName::DynArrayAlloc(id) => {
                write!(f, "DynArrayAlloc_{}", id.0)
            }

            FunctionName::BoxValue(id) => {
                write!(f, "BoxValue_{}", id.0)
            }
            
            FunctionName::Forget => write!(f, "Forget"),

            FunctionName::DynArrayBoundsCheck => write!(f, "DynArrayBoundsCheck"),

            FunctionName::Builtin(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum BuiltinName {
    RcNew,
    RcNewArray,
    RcRetain,
    RcRelease,
    IsImpl,
    Raise,
    InvokeMethod,

    FindTypeInfo,
    GetTypeInfoCount,
    GetTypeInfoByIndex,
    GetObjectTypeInfo,

    FindFuncInfo,
    GetFuncInfoCount,
    GetFuncInfoByIndex,
    InvokeFunction,

    Int8ToStr,
    ByteToStr,
    Int16ToStr,
    UInt16ToStr,
    IntToStr,
    UInt32ToStr,
    Int64ToStr,
    UInt64ToStr,
    NativeIntToStr,
    NativeUIntToStr,
    PointerToStr,
    Real32ToStr,
    Real64ToStr,

    StrToInt,
    Write,
    WriteLn,
    ReadLn,
    GetMem,
    FreeMem,
    ZeroMemory,

    ArrayLengthInternal,
    ArrayCreateInternal,

    RandomInteger,
    RandomSingle,

    Time,

    Pow,
    Sqrt,
    Sin,
    ArcSin,
    Cos,
    ArcCos,
    Tan,
    ArcTan,

    Infinity,
    IsInfinite,
    NaN,
    IsNaN,
}

impl fmt::Display for BuiltinName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BuiltinName::RcNew => write!(f, "RcNew"),
            BuiltinName::RcNewArray => write!(f, "RcNewArray"),
            BuiltinName::RcRetain => write!(f, "RcRetain"),
            BuiltinName::RcRelease => write!(f, "RcRelease"),
            BuiltinName::IsImpl => write!(f, "IsImpl"),
            BuiltinName::Raise => write!(f, "Raise"),
            BuiltinName::InvokeMethod => write!(f, "InvokeMethod"),
            BuiltinName::InvokeFunction => write!(f, "InvokeFunction"),

            BuiltinName::FindTypeInfo => write!(f, "System_FindTypeInfo"),
            BuiltinName::GetTypeInfoCount => write!(f, "System_GetTypeInfoCount"),
            BuiltinName::GetTypeInfoByIndex => write!(f, "System_GetTypeInfoByIndex"),
            BuiltinName::GetObjectTypeInfo => write!(f, "System_GetObjectTypeInfo"),

            BuiltinName::FindFuncInfo => write!(f, "System_FindFunctionInfo"),
            BuiltinName::GetFuncInfoCount => write!(f, "System_GetFunctionInfoCount"),
            BuiltinName::GetFuncInfoByIndex => write!(f, "System_GetFunctionInfoByIndex"),

            BuiltinName::WriteLn => write!(f, "System_WriteLn"),
            BuiltinName::Write => write!(f, "System_Write"),
            BuiltinName::ReadLn => write!(f, "System_ReadLn"),
            BuiltinName::StrToInt => write!(f, "System_StrToInt"),
            BuiltinName::GetMem => write!(f, "System_GetMem"),
            BuiltinName::FreeMem => write!(f, "System_FreeMem"),
            BuiltinName::ZeroMemory => write!(f, "System_ZeroMemory"),

            BuiltinName::Int8ToStr => write!(f, "System_Int8ToStr"),
            BuiltinName::ByteToStr => write!(f, "System_ByteToStr"),
            BuiltinName::Int16ToStr => write!(f, "System_Int16ToStr"),
            BuiltinName::UInt16ToStr => write!(f, "System_UInt16ToStr"),
            BuiltinName::IntToStr => write!(f, "System_IntToStr"),
            BuiltinName::UInt32ToStr => write!(f, "System_UInt32ToStr"),
            BuiltinName::Int64ToStr => write!(f, "System_Int64ToStr"),
            BuiltinName::UInt64ToStr => write!(f, "System_UInt64ToStr"),
            BuiltinName::NativeIntToStr => write!(f, "System_NativeIntToStr"),
            BuiltinName::NativeUIntToStr => write!(f, "System_NativeUIntToStr"),
            BuiltinName::PointerToStr => write!(f, "System_PointerToStr"),
            BuiltinName::Real32ToStr => write!(f, "System_Real32ToStr"),
            BuiltinName::Real64ToStr => write!(f, "System_Real64ToStr"),

            BuiltinName::ArrayLengthInternal => write!(f, "System_ArrayLengthInternal"),
            BuiltinName::ArrayCreateInternal => write!(f, "System_ArrayCreateInternal"),

            BuiltinName::RandomInteger => write!(f, "System_RandomInteger"),
            BuiltinName::RandomSingle => write!(f, "System_RandomSingle"),

            BuiltinName::Time => write!(f, "System_Time"),

            BuiltinName::Pow => write!(f, "System_Pow"),
            BuiltinName::Sqrt => write!(f, "System_Sqrt"),
            BuiltinName::Sin => write!(f, "System_Sin"),
            BuiltinName::ArcSin => write!(f, "System_ArcSin"),
            BuiltinName::Cos => write!(f, "System_Cos"),
            BuiltinName::ArcCos => write!(f, "System_ArcCos"),
            BuiltinName::Tan => write!(f, "System_Tan"),
            BuiltinName::ArcTan => write!(f, "System_ArcTan"),

            BuiltinName::Infinity => write!(f, "System_Infinity"),
            BuiltinName::IsInfinite => write!(f, "System_IsInfinite"),
            BuiltinName::NaN => write!(f, "System_NaN"),
            BuiltinName::IsNaN => write!(f, "System_IsNaN"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDecl {
    pub name: FunctionName,
    pub return_ty: Type,
    pub params: Vec<Type>,

    pub comment: Option<String>,
}

impl FunctionDecl {
    pub fn translate(
        def_id: ir::FunctionID,
        instance_id: FuncInstanceID,
        func: &ir::FunctionDef,
        module: &mut Unit,
    ) -> Self {
        let name = FunctionName::ID(instance_id);

        let return_ty = module.translate_type(&func.sig.result_type);
        let params = func
            .sig
            .param_types
            .iter()
            .map(|param| module.translate_type(param))
            .collect();

        let func_desc = module.metadata.func_desc(def_id);

        let mut comment = match func_desc {
            Some(name) => name.clone(),
            None => format!("function {}", instance_id),
        };

        comment.push_str(": (");
        for (i, arg) in func.sig.param_types.iter().enumerate() {
            if i > 0 {
                comment.push_str(", ");
            }
            comment.push_str(&arg.to_pretty_string(module.metadata));
        }
        comment.push_str(") -> ");
        comment.push_str(&func.sig.result_type.to_pretty_string(module.metadata));

        Self {
            name,
            return_ty,
            params,
            comment: Some(comment),
        }
    }

    pub fn ptr_type(&self) -> Type {
        Type::FunctionPointer {
            return_ty: Box::new(self.return_ty.clone()),
            params: self.params.clone(),
        }
    }
}

impl fmt::Display for FunctionDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(comment) = &self.comment {
            writeln!(f, "/** {} **/", comment)?;
        }

        let name = self.name.to_string();

        write!(f, "{}(", self.return_ty.to_decl_string(&name))?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            let arg_id = ir::ArgID(i);
            let arg_name = VariableID::Arg(arg_id).to_string();

            write!(f, "{}", param.to_decl_string(&arg_name))?;
        }
        write!(f, ")")
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub decl: FunctionDecl,
    pub body: Vec<Statement>,
}

impl FunctionDef {
    pub fn translate(
        def_id: ir::FunctionID,
        instance_id: FuncInstanceID,
        func: &ir::FunctionDef,
        module: &mut Unit,
    ) -> Self {
        let result_type = func.sig.result_type.clone();
        let mut builder = CBuilder::new(module, &func.sig.param_types, result_type);

        builder.translate_instructions(&func.body);

        Self {
            body: builder.stmts,
            decl: FunctionDecl::translate(def_id, instance_id, func, module),
        }
    }
}

impl fmt::Display for FunctionDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{} {{", self.decl)?;

        if self.decl.return_ty != Type::Void {
            let result_var_name = VariableID::Result.to_string();
            writeln!(f, "{};", self.decl.return_ty.to_decl_string(&result_var_name))?;
        }

        for stmt in &self.body {
            writeln!(f, "{}", stmt)?;
        }

        if self.decl.return_ty != Type::Void {
            write!(f, "{}", Statement::ReturnValue(Expr::result_var()))?;
        }

        write!(f, "}}")
    }
}

pub struct FfiFunction {
    pub decl: FunctionDecl,
    pub symbol: String,
    pub src: String,
}

impl FfiFunction {
    pub fn translate(
        id: ir::FunctionID,
        func_ref: &ir::ExternalFunctionRef,
        module: &mut Unit,
    ) -> Self {
        let return_ty = module.translate_type(&func_ref.sig.result_type);
        let mut params = Vec::new();
        for param in &func_ref.sig.param_types {
            params.push(module.translate_type(param));
        }

        let decl = FunctionDecl {
            name: FunctionName::Extern(id),
            return_ty,
            params,
            comment: Some(format!(
                "external func {}::{}",
                func_ref.src, func_ref.symbol
            )),
        };

        FfiFunction {
            decl,
            src: func_ref.src.clone(),
            symbol: func_ref.symbol.clone(),
        }
    }

    pub fn init_statement(&self) -> Statement {
        let lib_filename = format!("{}{}{}", env::consts::DLL_PREFIX, self.src, env::consts::DLL_SUFFIX);

        Statement::Expr(
            Expr::Function(self.decl.name).assign_from(
                Expr::Function(FunctionName::LoadSymbol).call([
                    Expr::LitCString(lib_filename),
                    Expr::LitCString(self.symbol.clone()),
                ])
            )
        )
    }

    pub fn func_ptr_decl(&self) -> String {
        self.decl.ptr_type().to_decl_string(&self.decl.name)
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct FuncAliasDef {
    pub decl: TypeDecl,

    pub param_tys: Vec<Type>,
    pub return_ty: Type,

    pub comment: Option<String>,
}

impl fmt::Display for FuncAliasDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(comment) = &self.comment {
            write!(f, "/** {} */", comment)?;
        }

        let ptr_type = self.to_pointer_type();
        let decl_string = ptr_type.to_decl_string(&self.decl.name.to_string());

        write!(f, "typedef {}", decl_string)?;

        Ok(())
    }
}

impl FuncAliasDef {
    pub fn to_pointer_type(&self) -> Type {
        Type::FunctionPointer {
            return_ty: Box::new(self.return_ty.clone()),
            params: self.param_tys.clone(),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct FuncAliasID(pub ir::TypeDefID);

impl fmt::Display for FuncAliasID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FunctionAlias_{}", self.0 .0)
    }
}

impl<'a> Unit<'a> {
    pub fn translate_func_ref(&mut self, key: &ir::FunctionRef) -> FunctionInstance {
        if let Some(instance) = self.function_refs.get(key) {
            return *instance;
        }

        // eprintln!("translating func ref: {}", key.to_pretty_string(self.metadata));

        let instance_id = FuncInstanceID(self.function_refs.len());
        let instance = FunctionInstance {
            id: instance_id,
            name: FunctionName::ID(instance_id),
        };

        self.function_refs.insert(key.clone(), instance);
        instance
    }

    pub fn add_builtin_func(&mut self, id: ir::FunctionID, name: BuiltinName) -> FuncInstanceID {
        let key = ir::FunctionRef::new(id);
        if let Some(instance) = self.function_refs.get(&key) {
            return instance.id;
        }

        let instance_id = FuncInstanceID(self.function_refs.len());
        self.function_refs.insert(key, FunctionInstance {
            id: instance_id,
            name: FunctionName::Builtin(name),
        });

        instance_id
    }

    pub fn translate_extern_func_ref(
        &mut self,
        id: ir::FunctionID,
        extern_ref: &ir::ExternalFunctionRef,
    ) -> FuncInstanceID {
        let key = ir::FunctionRef::new(id);
        if let Some(instance) = self.function_refs.get(&key) {
            return instance.id;
        }

        let ffi_func = FfiFunction::translate(id, extern_ref, self);
        self.ffi_funcs.push(ffi_func);

        let instance_id = FuncInstanceID(self.function_refs.len());
        self.function_refs.insert(key, FunctionInstance {
            id: instance_id,
            name: FunctionName::Extern(id),
        });

        instance_id
    }

    pub fn build_func_ref(
        &mut self,
        instance_id: FuncInstanceID,
        func_ref: ir::FunctionRef,
        generic_def: &ir::FunctionDef,
    ) {
        let Some(func_info) = self.metadata.get_function_info(func_ref.def_id) else {
            let func_display = func_ref.to_pretty_string(self.metadata);
            panic!("build_func_ref: missing function info for {}", func_display);
        };

        let def_type_params = func_info.identity.type_params();

        if func_ref.args.len() != def_type_params.len() {
            panic!(
                "build_func_ref: incorrect number of type parameters for {} (invocation has {}, expected: {})",
                func_ref.to_pretty_string(self.metadata),
                func_ref.args.len(),
                def_type_params.len(),
            );
        }

        let def = if func_ref.args.len() == 0 {
            Cow::Borrowed(generic_def)
        } else {
            let types = build_invocation_type_map(&func_info.identity, &func_ref.args, self.metadata);

            let mut builder = FuncInstanceBuilder::new(self);
            let sig = instantiate_sig(&generic_def.sig, &types);

            instantiate_function_def(&generic_def, def_type_params, &types, &mut builder);

            let body = builder.finish();

            Cow::Owned(ir::FunctionDef {
                body,
                sig: Rc::new(sig),
            })
        };

        let c_def = FunctionDef::translate(func_ref.def_id, instance_id, &def, self);
        self.functions.push(c_def);
    }
}