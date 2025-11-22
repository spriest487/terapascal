use crate::ast::boxed::BoxTypeID;
use crate::ast::Builder;
use crate::ast::DynArrayTypeID;
use crate::ast::Expr;
use crate::ast::Statement;
use crate::ast::Type;
use crate::ast::TypeDecl;
use crate::ast::TypeDefName;
use crate::ast::Unit;
use std::env;
use std::fmt;
use terapascal_ir as ir;

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum FunctionName {
    // c main function
    Main,

    // init function that all loaded modules append their init code into
    Init,

    // function to initialize an external symbol at runtime
    LoadSymbol,

    ID(ir::FunctionID),
    Method(ir::InterfaceID, ir::MethodID),
    DestructorInvoker(TypeDefName),
    MethodWrapper(ir::InterfaceID, ir::MethodID, TypeDefName),

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
            FunctionName::DestructorInvoker(name) => write!(f, "Destructor_{}", name),

            FunctionName::Method(iface, method) => {
                write!(f, "Method_{}_{}", iface, method.0)
            },
            FunctionName::MethodWrapper(iface, method, self_ty) => {
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
    RealToStr,

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
            BuiltinName::RealToStr => write!(f, "System_RealToStr"),

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
    pub fn translate(id: ir::FunctionID, func: &ir::FunctionDef, module: &mut Unit) -> Self {
        let name = FunctionName::ID(id);
        let return_ty = Type::from_metadata(&func.sig.return_ty, module);
        let params = func
            .sig
            .param_tys
            .iter()
            .map(|param| Type::from_metadata(param, module))
            .collect();

        let mut comment = match &func.debug_name {
            Some(name) => name.clone(),
            None => format!("function {}", id),
        };

        comment.push_str(": (");
        for (i, arg) in func.sig.param_tys.iter().enumerate() {
            if i > 0 {
                comment.push_str(", ");
            }
            comment.push_str(&arg.to_string());
        }
        comment.push_str(") -> ");
        comment.push_str(&func.sig.return_ty.to_string());

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

            let arg_id = if self.return_ty != Type::Void {
                i + 1
            } else {
                i
            };

            let name = format!("L{}", arg_id);
            write!(f, "{}", param.to_decl_string(&name))?;
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
    pub fn translate(id: ir::FunctionID, func: &ir::FunctionDef, module: &mut Unit) -> Self {
        let mut builder = Builder::new(module);
        builder.translate_instructions(&func.body);

        Self {
            body: builder.stmts,
            decl: FunctionDecl::translate(id, func, module),
        }
    }
}

impl fmt::Display for FunctionDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{} {{", self.decl)?;

        if self.decl.return_ty != Type::Void {
            writeln!(f, "{};", self.decl.return_ty.to_decl_string("L0"))?;
        }

        for stmt in &self.body {
            writeln!(f, "{}", stmt)?;
        }

        if self.decl.return_ty != Type::Void {
            write!(f, "{}", Statement::ReturnValue(Expr::local_var(ir::RETURN_LOCAL)))?;
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
        let return_ty = Type::from_metadata(&func_ref.sig.return_ty, module);
        let mut params = Vec::new();
        for param in &func_ref.sig.param_tys {
            params.push(Type::from_metadata(param, module));
        }

        let decl = FunctionDecl {
            name: FunctionName::ID(id),
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
