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

    Builtin(BuiltinName),
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

            FunctionName::Builtin(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum BuiltinName {
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

impl fmt::Display for BuiltinName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BuiltinName::RcAlloc => write!(f, "RcAlloc"),
            BuiltinName::RcRetain => write!(f, "RcRetain"),
            BuiltinName::RcRelease => write!(f, "RcRelease"),
            BuiltinName::IsImpl => write!(f, "IsImpl"),
            BuiltinName::Raise => write!(f, "Raise"),
            BuiltinName::InvokeMethod => write!(f, "InvokeMethod"),
            BuiltinName::InvokeFunc => write!(f, "InvokeFunction"),

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
            BuiltinName::ArraySetLengthInternal => write!(f, "System_ArraySetLengthInternal"),

            BuiltinName::RandomInteger => write!(f, "System_RandomInteger"),
            BuiltinName::RandomSingle => write!(f, "System_RandomSingle"),

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
        let param_tys: Vec<_> = func_def.sig.param_tys
            .iter()
            .map(|ir_ty| Type::from_metadata(ir_ty, module))
            .collect();
        
        let return_ty = Type::from_metadata(&func_def.sig.return_ty, module);
        
        Self::new_invoker(
            module,
            FunctionName::Invoker(id),
            FunctionName::ID(id),
            &param_tys,
            &return_ty
        )
    }

    pub fn invoker_builtin(
        name: BuiltinName,
        func_id: ir::FunctionID,
        param_tys: &[Type],
        return_ty: &Type,
        module: &mut Unit
    ) -> Self {
        Self::new_invoker(
            module,
            FunctionName::Invoker(func_id),
            FunctionName::Builtin(name),
            &param_tys,
            &return_ty
        )
    }

    fn new_invoker(
        module: &mut Unit, 
        invoker_name: FunctionName,
        func_name: FunctionName,
        param_tys: &[Type], 
        return_ty: &Type
    ) -> Self {
        let mut builder = Builder::new(module);

        let args_array = ir::LocalID(0);
        let result_ptr = ir::LocalID(1);

        let mut arg_vars = Vec::new();

        for i in 0..param_tys.len() {
            let param_ty = &param_tys[i];
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
                ).deref().cast(param_ty.clone().ptr()).deref(),
            );

            arg_vars.push(Expr::Variable(arg_var));
        }

        let function = Expr::Function(func_name);

        if *return_ty == Type::Void {
            builder.stmts.push(Statement::Expr(function.call(arg_vars)));
        } else {
            // *((TReturn*) resultPtr) = function(..); 
            let result_ptr_expr = Expr::local_var(result_ptr)
                .cast(return_ty.clone().ptr())
                .deref();

            builder.assign(result_ptr_expr, function.call(arg_vars));
        }

        Self {
            body: builder.stmts,
            decl: FunctionDecl {
                comment: Some(format!("runtime invoker for function {func_name}")),
                name: invoker_name,
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
