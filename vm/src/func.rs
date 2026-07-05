use crate::func::ffi::FfiInvoker;
use crate::ir;
use crate::marshal::MarshalResult;
use crate::marshal::Marshaller;
use crate::result::ExecError;
use crate::ExecResult;
use crate::GlobalValue;
use crate::Vm;
use ir::generic::*;
use ir::FunctionName;
use ir::MetadataSource as _;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::SharedStringKey;

pub mod ffi;

pub type BuiltinFn = fn(state: &mut Vm) -> ExecResult<()>;

pub struct BuiltinFunction {
    pub func: BuiltinFn,
    pub return_ty: ir::Type,
    pub param_tys: Vec<ir::Type>,

    pub name: Rc<String>,
}

pub struct FfiFunction {
    name: Rc<String>,

    return_ty: ir::Type,
    param_tys: Vec<ir::Type>,

    invoker: FfiInvoker,
}

pub struct IRFunction {
    def: ir::FunctionDef,
    name: Rc<String>,
}

pub enum Function {
    Builtin(BuiltinFunction),
    External(FfiFunction),
    IR(IRFunction),
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub function: Rc<Function>,

    pub identity: ir::FunctionIdentity,

    pub invoker: Option<ir::FunctionID>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct FuncInstanceID(pub usize);

impl Function {
    pub fn new(id: ir::FunctionID, def: ir::FunctionDef, metadata: &ir::Metadata) -> Self {
        let func_info = metadata.get_function_info(id);

        let identity = func_info
            .map(|func_info| func_info.identity.clone())
            .unwrap_or_else(|| {
                let internal_name = Function::make_debug_name(
                    &id.to_string(),
                    &def.sig.param_types,
                    metadata,
                );
                ir::FunctionIdentity::internal(internal_name, [])
            });

        let name = identity.to_pretty_string(metadata).into_owned();

        Self::IR(IRFunction {
            name: Rc::new(name),
            def: def.clone(),
        })
    }

    pub fn new_internal(name: impl Into<Rc<String>>, def: ir::FunctionDef) -> Self {
        let name = name.into();

        Self::IR(IRFunction {
            name,
            def,
        })
    }

    pub fn name(&self) -> &Rc<String> {
        match self {
            Function::Builtin(f) => &f.name,
            Function::External(f) => &f.name,
            Function::IR(f) => &f.name,
        }
    }

    pub fn new_ffi(
        func_ref: &ir::ExternalFunctionRef,
        marshaller: &mut Marshaller,
    ) -> MarshalResult<Self> {
        let invoker = marshaller.build_ffi_invoker(&func_ref)?;

        let symbol_name =  format!("{}::{}", func_ref.src, func_ref.symbol);
        let name = Self::make_debug_name(&symbol_name, &func_ref.sig.param_types, marshaller.metadata());

        let func = Function::External(FfiFunction {
            name: Rc::new(name),
            return_ty: func_ref.sig.result_type.clone(),
            param_tys: func_ref.sig.param_types.clone(),

            invoker,
        });

        Ok(func)
    }

    pub fn return_ty(&self) -> &ir::Type {
        match self {
            Function::Builtin(func) => &func.return_ty,
            Function::External(func) => &func.return_ty,
            Function::IR(func) => &func.def.sig.result_type,
        }
    }

    pub fn param_tys(&self) -> &[ir::Type] {
        match self {
            Function::Builtin(builtin_fn) => &builtin_fn.param_tys,
            Function::External(external_fn) => &external_fn.param_tys,
            Function::IR(func) => &func.def.sig.param_types,
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
                    println!("[vm] calling {} (builtin)", def.name);
                }
                (def.func)(state)?
            }

            Function::External(def) => {
                def.invoker.invoke(state)?
            },
            
            Function::IR(func) => {
                if state.opts().trace_ir {
                    println!("[vm] entering {}", func.name);
                }

                state.execute(&func.def.body)?;

                if state.opts().trace_ir {
                    println!("[vm] exiting {}", func.name);
                }
            }
        };

        Ok(())
    }

    pub fn stack_alloc_size(&self, marshaller: &mut Marshaller) -> MarshalResult<usize> {
        let mut args_size = 0;
        for arg_ty in self.param_tys() {
            let arg_size = marshaller.create_native_type(arg_ty)?.size();
            args_size += arg_size;
        }

        let return_size = match self.return_ty() {
            ir::Type::Nothing => 0,
            return_ty => {
                marshaller.create_native_type(return_ty)?.size()
            },
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
            Function::Builtin(func) => write!(f, "<native function: {}>", func.name),
            Function::IR(func) => write!(f, "<function: {}>", func.name),
            Function::External(func) => write!(f, "<ffi function: {}>", func.name),
        }
    }
}

pub fn instantiate_func(
    vm: &mut Vm,
    func_ref: &ir::FunctionRef,
) -> ExecResult<FunctionInfo> {
    if let Some(existing_instance) = vm.functions.get(func_ref) {
        return Ok(existing_instance.clone());
    }

    let Some(func_info) = vm.metadata().get_function_info(func_ref.def_id) else {
        let func_ref_display = func_ref.to_pretty_string(vm.metadata());
        let msg = format!("missing function metadata: {func_ref_display}");
        return Err(ExecError::illegal_state(msg))
    };

    let def_type_params = func_info.identity.type_params();

    let generic_instance = vm.functions
        .get(&ir::FunctionRef::new(func_ref.def_id))
        .ok_or_else(|| {
            let func_ref_display = func_ref.to_pretty_string(vm.metadata());
            let msg = format!("missing generic function definition: {func_ref_display}");
            ExecError::illegal_state(msg)
        })?;

    let generic_def = match generic_instance.function.as_ref() {
        Function::IR(func) => &func.def,
        _ => {
            let func_display = func_ref.to_pretty_string(vm.metadata());
            let msg = format!("definition of generic function {func_display} must not be external");
            return Err(ExecError::illegal_state(msg))
        }
    };

    let invocation_type_params = invocation_type_params(&func_info.identity, vm.metadata());

    if func_ref.args.len() != invocation_type_params.len() {
        let msg = format!(
            "incorrect number of type parameters for function {} (invocation has {}, expected: {})",
            generic_instance.identity.to_pretty_string(vm.metadata()),
            func_ref.args.len(),
            invocation_type_params.len(),
        );
        return Err(ExecError::illegal_state(msg))
    }

    let mut types = HashMap::with_capacity(invocation_type_params.len());
    build_type_map(invocation_type_params.iter().map(Cow::as_ref), &func_ref.args, &mut types);

    let identity = match &generic_instance.identity {
        ir::FunctionIdentity::Internal { name, type_params } => {
            let type_params: Vec<_> = type_params
                .iter()
                .map(|t| instantiate_type_param(t, &types))
                .collect();

            let instance_name = if !type_params.is_empty() {
                let mut name = name.to_string();
                name.push_str(&format_type_arg_list(&type_params, &types, vm.metadata()));
                Arc::new(name)
            } else {
                name.clone()
            };

            ir::FunctionIdentity::Internal {
                name: instance_name,
                type_params,
            }
        }

        ir::FunctionIdentity::Destructor { declaring_type, id, name } => {
            let declaring_type = instantiate_type(declaring_type, &types);

            ir::FunctionIdentity::Destructor {
                declaring_type,
                id: *id,
                name: name.clone(),
            }
        }

        ir::FunctionIdentity::Method { declaring_type, id, name, type_params} => {
            let declaring_type = instantiate_type(declaring_type, &types);
            let type_params = type_params.iter().map(|t| instantiate_type_param(t, &types)).collect();

            ir::FunctionIdentity::Method {
                declaring_type,
                id: *id,
                name: name.clone(),
                type_params,
            }
        }

        ir::FunctionIdentity::Global(func_name) => {
            ir::FunctionIdentity::Global(FunctionName {
                path: func_name.path.clone(),
                type_params: func_name.type_params
                    .iter()
                    .map(|p| instantiate_type_param(p, &types))
                    .collect(),
            })
        }
    };

    let func_name = Rc::new(identity.to_pretty_string(vm.metadata()).into_owned());

    if vm.opts.trace_generics {
        eprintln!(
            "[vm] new instantiation of function {}: {}",
            generic_instance.identity.to_pretty_string(vm.metadata()),
            func_name,
        );
    }

    let mut builder = ir::RawInstructionBuilder::new(vm.metadata(), true);
    let sig = instantiate_sig(&generic_def.sig, &types);

    instantiate_function_def(&generic_def, def_type_params, &types, &mut builder);

    let def = ir::FunctionDef {
        body: builder.finish(),
        sig: Rc::new(sig),
    };

    let func_info = FunctionInfo {
        function: Rc::new(Function::IR(IRFunction {
            name: func_name,
            def,
        })),
        identity,
        invoker: None,
    };

    vm.functions.insert(func_ref.clone(), func_info.clone());
    vm.globals.insert(ir::GlobalRef::Function(func_ref.clone()), GlobalValue::Function(func_ref.clone()));

    Ok(func_info)
}

fn format_type_arg_list(
    params: &[ir::TypeParam],
    types: &HashMap<SharedStringKey, ir::Type>,
    formatter: &impl ir::IRFormatter,
) -> String {
    let mut result = String::from("[");

    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }

        result.push_str(param.name.as_str());
        result.push_str("=");
        result.push_str(&types[&param.name].to_pretty_string(formatter));
    }

    result.push_str("]");

    result
}