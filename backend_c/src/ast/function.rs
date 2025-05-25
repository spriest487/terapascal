use crate::ast::{Builder, VariableID};
use crate::ast::Expr;
use crate::ast::InfixOp;
use crate::ast::Statement;
use crate::ast::Type;
use crate::ast::TypeDecl;
use crate::ast::Unit;
use ir_lang as ir;
use std::fmt;

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum FunctionName {
    // c main function
    Main,

    // init function that all loaded modules append their init code into
    Init,

    // function to initialize an external symbol at runtime
    LoadSymbol,

    ID(ir::FunctionID),
    Invoker(ir::FunctionID),
    Method(ir::InterfaceID, ir::MethodID),
    DestructorInvoker(ir::TypeDefID),
    MethodWrapper(ir::InterfaceID, ir::MethodID, ir::TypeDefID),

    // runtime functions
    RcAlloc,
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
    InvokeFunc,

    // builtins
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

    ArrayLengthInternal,
    ArraySetLengthInternal,
    
    RandomInteger,
    RandomSingle,
    
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

impl fmt::Display for FunctionName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FunctionName::Main => write!(f, "main"),
            FunctionName::Init => write!(f, "ModuleInit"),
            FunctionName::LoadSymbol => write!(f, "LoadSymbol"),

            FunctionName::ID(id) => write!(f, "Function_{}", id.0),
            FunctionName::Invoker(id) => write!(f, "Invoker_{}", id.0),
            FunctionName::DestructorInvoker(id) => write!(f, "Destructor_{}", id.0),

            FunctionName::Method(iface, method) => write!(f, "Method_{}_{}", iface, method.0),
            FunctionName::MethodWrapper(iface, method, self_ty) => {
                write!(f, "Method_{}_{}_Wrapper_{}", iface, method.0, self_ty)
            },

            FunctionName::RcAlloc => write!(f, "RcAlloc"),
            FunctionName::RcRetain => write!(f, "RcRetain"),
            FunctionName::RcRelease => write!(f, "RcRelease"),
            FunctionName::IsImpl => write!(f, "IsImpl"),
            FunctionName::Raise => write!(f, "Raise"),
            FunctionName::InvokeMethod => write!(f, "InvokeMethod"),
            FunctionName::InvokeFunc => write!(f, "InvokeFunction"),

            FunctionName::FindTypeInfo => write!(f, "System_FindTypeInfo"),
            FunctionName::GetTypeInfoCount => write!(f, "System_GetTypeInfoCount"),
            FunctionName::GetTypeInfoByIndex => write!(f, "System_GetTypeInfoByIndex"),
            FunctionName::GetObjectTypeInfo => write!(f, "System_GetObjectTypeInfo"),

            FunctionName::FindFuncInfo => write!(f, "System_FindFunctionInfo"),
            FunctionName::GetFuncInfoCount => write!(f, "System_GetFunctionInfoCount"),
            FunctionName::GetFuncInfoByIndex => write!(f, "System_GetFunctionInfoByIndex"),

            FunctionName::WriteLn => write!(f, "System_WriteLn"),
            FunctionName::Write => write!(f, "System_Write"),
            FunctionName::ReadLn => write!(f, "System_ReadLn"),
            FunctionName::StrToInt => write!(f, "System_StrToInt"),
            FunctionName::GetMem => write!(f, "System_GetMem"),
            FunctionName::FreeMem => write!(f, "System_FreeMem"),

            FunctionName::Int8ToStr => write!(f, "System_Int8ToStr"),
            FunctionName::ByteToStr => write!(f, "System_ByteToStr"),
            FunctionName::Int16ToStr => write!(f, "System_Int16ToStr"),
            FunctionName::UInt16ToStr => write!(f, "System_UInt16ToStr"),
            FunctionName::IntToStr => write!(f, "System_IntToStr"),
            FunctionName::UInt32ToStr => write!(f, "System_UInt32ToStr"),
            FunctionName::Int64ToStr => write!(f, "System_Int64ToStr"),
            FunctionName::UInt64ToStr => write!(f, "System_UInt64ToStr"),
            FunctionName::NativeIntToStr => write!(f, "System_NativeIntToStr"),
            FunctionName::NativeUIntToStr => write!(f, "System_NativeUIntToStr"),
            FunctionName::PointerToStr => write!(f, "System_PointerToStr"),
            FunctionName::RealToStr => write!(f, "System_RealToStr"),

            FunctionName::ArrayLengthInternal => write!(f, "System_ArrayLengthInternal"),
            FunctionName::ArraySetLengthInternal => write!(f, "System_ArraySetLengthInternal"),
            
            FunctionName::RandomInteger => write!(f, "System_RandomInteger"),
            FunctionName::RandomSingle => write!(f, "System_RandomSingle"),

            FunctionName::Pow => write!(f, "System_Pow"),
            FunctionName::Sqrt => write!(f, "System_Sqrt"),
            FunctionName::Sin => write!(f, "System_Sin"),
            FunctionName::ArcSin => write!(f, "System_ArcSin"),
            FunctionName::Cos => write!(f, "System_Cos"),
            FunctionName::ArcCos => write!(f, "System_ArcCos"),
            FunctionName::Tan => write!(f, "System_Tan"),
            FunctionName::ArcTan => write!(f, "System_ArcTan"),
            
            FunctionName::Infinity => write!(f, "System_Infinity"),
            FunctionName::IsInfinite => write!(f, "System_IsInfinite"),
            FunctionName::NaN => write!(f, "System_NaN"),
            FunctionName::IsNaN => write!(f, "System_IsNaN"),
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
    pub fn translate(id: ir::FunctionID, func: &ir_lang::FunctionDef, module: &mut Unit) -> Self {
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
    
    pub fn invoker(id: ir::FunctionID, func_def: &ir::FunctionDef, module: &mut Unit) -> Self {
        let mut builder = Builder::new(module);

        let args_array = ir::LocalID(0);
        let result_ptr = ir::LocalID(1);
        
        let mut arg_vars = Vec::new();
        
        for i in 0..func_def.sig.param_tys.len() {
            let param_ty = builder.translate_type(&func_def.sig.param_tys[i]);
            let arg_var = VariableID::Named(Box::new(format!("arg{i}")));
 
            builder.stmts.push(Statement::VariableDecl {
                id: arg_var.clone(),
                null_init: false,
                ty: param_ty.clone(),
            });

            // argN := *((TArg*) *(args + i));
            builder.assign(
                Expr::Variable(arg_var.clone()),
                Expr::infix_op(
                    Expr::local_var(args_array), 
                    InfixOp::Add,
                    Expr::LitInt(i as i128),
                ).deref().cast(param_ty.ptr()).deref(),
            );
            
            arg_vars.push(Expr::Variable(arg_var));
        }
        
        let function = Expr::Function(FunctionName::ID(id));

        if func_def.sig.return_ty == ir::Type::Nothing {
            builder.stmts.push(Statement::Expr(function.call(arg_vars)));
        } else {
            let result_ty = builder.translate_type(&func_def.sig.return_ty);
            
            // *((TReturn*) resultPtr) = function(..); 
            let result_ptr_expr = Expr::local_var(result_ptr)
                .cast(result_ty.ptr())
                .deref();

            builder.assign(result_ptr_expr, function.call(arg_vars));
        }
        
        Self {
            body: builder.stmts,
            decl: FunctionDecl {
                comment: Some(format!("reflection invoker for function {id}")),
                name: FunctionName::Invoker(id),
                params: vec![
                    Type::Void.ptr().ptr(), // args
                    Type::Void.ptr(), // result
                ],
                return_ty: Type::Void,
            }
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
        Statement::Expr(Expr::InfixOp {
            lhs: Expr::Function(self.decl.name).into(),
            op: InfixOp::Assign,
            rhs: Expr::Call {
                func: Expr::Function(FunctionName::LoadSymbol).into(),
                args: vec![
                    Expr::LitCString(self.src.clone()),
                    Expr::LitCString(self.symbol.clone()),
                ],
            }
            .into(),
        })
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
