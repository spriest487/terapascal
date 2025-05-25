use crate::func::ffi::FfiInvoker;
use crate::ir;
use crate::marshal::MarshalResult;
use crate::marshal::Marshaller;
use crate::ExecResult;
use crate::Interpreter;
use std::fmt;

pub mod ffi;

pub type BuiltinFn = fn(state: &mut Interpreter) -> ExecResult<()>;

pub struct BuiltinFunction {
    pub func: BuiltinFn,
    pub return_ty: ir::Type,
    pub param_tys: Vec<ir::Type>,

    pub debug_name: String,
}

pub struct FfiFunction {
    debug_name: String,

    return_ty: ir::Type,
    param_tys: Vec<ir::Type>,

    invoker: FfiInvoker,
}

pub enum Function {
    Builtin(BuiltinFunction),
    External(FfiFunction),
    IR(ir::FunctionDef),
}

impl Function {
    pub fn new_ffi(
        func_ref: &ir::ExternalFunctionRef,
        marshaller: &mut Marshaller,
        metadata: &ir::Metadata,
    ) -> MarshalResult<Self> {
        let invoker = marshaller.build_ffi_invoker(&func_ref, metadata)?;

        let func = Function::External(FfiFunction {
            debug_name: format!("{}::{}", func_ref.src, func_ref.symbol),
            return_ty: func_ref.sig.return_ty.clone(),
            param_tys: func_ref.sig.param_tys.clone(),

            invoker,
        });

        Ok(func)
    }

    pub fn return_ty(&self) -> &ir::Type {
        match self {
            Function::Builtin(def) => &def.return_ty,
            Function::External(def) => &def.return_ty,
            Function::IR(def) => &def.sig.return_ty,
        }
    }

    pub fn param_tys(&self) -> &[ir::Type] {
        match self {
            Function::Builtin(builtin_fn) => &builtin_fn.param_tys,
            Function::External(external_fn) => &external_fn.param_tys,
            Function::IR(func_def) => &func_def.sig.param_tys,
        }
    }

    pub fn debug_name(&self) -> Option<&str> {
        match self {
            Function::Builtin(def) => Some(def.debug_name.as_str()),
            Function::External(def) => Some(def.debug_name.as_str()),
            Function::IR(def) => def.debug_name.as_ref().map(|s| s.as_str()),
        }
    }

    pub fn invoke(&self, state: &mut Interpreter) -> ExecResult<()> {
        match self {
            Function::Builtin(def) => {
                if state.opts().trace_ir {
                    println!("calling {} (interpreter builtin)", def.debug_name);
                }
                (def.func)(state)?
            }

            Function::External(def) => def.invoker.invoke(state)?,
            
            Function::IR(def) => {
                let display_name = match &def.debug_name {
                    Some(name) => name.as_str(),
                    None => "function",
                };

                if state.opts().trace_ir {
                    println!("entering {}", display_name);
                }
                state.execute(&def.body)?;
                if state.opts().trace_ir {
                    println!("exiting {}", display_name);
                }
            }
        };

        Ok(())
    }

    pub fn stack_alloc_size(&self, marshaller: &Marshaller) -> MarshalResult<usize> {
        let mut args_size = 0;
        for arg_ty in self.param_tys() {
            let arg_size = marshaller.get_ty(arg_ty)?.size();
            args_size += arg_size;
        }

        let return_size = match self.return_ty() {
            ir::Type::Nothing => 0,
            return_ty => marshaller.get_ty(return_ty)?.size(),
        };

        match self {
            Function::IR(def) => {
                let body_size = marshaller.stack_alloc_size(&def.body)?;
                Ok(body_size + args_size + return_size)
            }

            Function::Builtin(..) | Function::External(..) => Ok(args_size + return_size),
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin { .. } => write!(f, "<native code>"),
            Function::IR(func) => write!(f, "<function with {} instructions>", func.body.len()),
            Function::External(func) => write!(f, "<{}>", func.debug_name),
        }
    }
}
