use crate::func::ffi::FfiInvoker;
use crate::ir;
use crate::marshal::MarshalResult;
use crate::marshal::Marshaller;
use crate::ExecResult;
use crate::Vm;
use std::fmt;

pub mod ffi;

pub type BuiltinFn = fn(state: &mut Vm) -> ExecResult<()>;

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

pub struct IRFunction {
    def: ir::FunctionDef,

    debug_name: String,
}

pub enum Function {
    Builtin(BuiltinFunction),
    External(FfiFunction),
    IR(IRFunction),
}

impl Function {
    pub fn new(id: ir::FunctionID, def: ir::FunctionDef, metadata: &ir::Metadata) -> Self {
        let func_name = def.debug_name
            .clone()
            .unwrap_or_else(|| id.to_string());
        let debug_name = Function::make_debug_name(
            &func_name,
            &def.sig.param_tys,
            metadata,
        );

        Self::IR(IRFunction {
            debug_name,
            def: def.clone(),
        })
    }

    pub fn new_ffi(
        func_ref: &ir::ExternalFunctionRef,
        marshaller: &mut Marshaller,
        metadata: &ir::Metadata,
    ) -> MarshalResult<Self> {
        let invoker = marshaller.build_ffi_invoker(&func_ref, metadata)?;

        let func_name =  format!("{}::{}", func_ref.src, func_ref.symbol);
        let debug_name = Self::make_debug_name(&func_name, &func_ref.sig.param_tys, metadata);

        let func = Function::External(FfiFunction {
            debug_name,
            return_ty: func_ref.sig.return_ty.clone(),
            param_tys: func_ref.sig.param_tys.clone(),

            invoker,
        });

        Ok(func)
    }

    pub fn return_ty(&self) -> &ir::Type {
        match self {
            Function::Builtin(func) => &func.return_ty,
            Function::External(func) => &func.return_ty,
            Function::IR(func) => &func.def.sig.return_ty,
        }
    }

    pub fn param_tys(&self) -> &[ir::Type] {
        match self {
            Function::Builtin(builtin_fn) => &builtin_fn.param_tys,
            Function::External(external_fn) => &external_fn.param_tys,
            Function::IR(func) => &func.def.sig.param_tys,
        }
    }

    pub fn debug_name(&self) -> &str {
        match self {
            Function::Builtin(def) => def.debug_name.as_str(),
            Function::External(def) => def.debug_name.as_str(),
            Function::IR(func) => func.debug_name.as_str(),
        }
    }

    fn make_debug_name(
        func_name: &str,
        params: &[ir::Type],
        formatter: &impl ir::IRFormatter,
    ) -> String {
        let mut name = format!("{}", func_name);

        if !params.is_empty() {
            name.push('(');
            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    name.push_str("; ");
                }

                name.push_str(&param.to_pretty_string(formatter));
            }
            name.push(')');
        }

        name
    }

    pub fn invoke(&self, state: &mut Vm) -> ExecResult<()> {
        match self {
            Function::Builtin(def) => {
                if state.opts().trace_ir {
                    println!("[vm] calling {} (builtin)", def.debug_name);
                }
                (def.func)(state)?
            }

            Function::External(def) => def.invoker.invoke(state)?,
            
            Function::IR(func) => {
                if state.opts().trace_ir {
                    println!("[vm] entering {}", func.debug_name);
                }

                state.execute(&func.def.body)?;

                if state.opts().trace_ir {
                    println!("[vm] exiting {}", func.debug_name);
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
            Function::IR(func) => {
                let body_size = marshaller.stack_alloc_size(&func.def.body.instructions)?;
                Ok(body_size + args_size + return_size)
            }

            Function::Builtin(..) | Function::External(..) => Ok(args_size + return_size),
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::Builtin(func) => write!(f, "<native function: {}>", func.debug_name),
            Function::IR(func) => write!(f, "<function: {}>", func.debug_name),
            Function::External(func) => write!(f, "<ffi function: {}>", func.debug_name),
        }
    }
}
