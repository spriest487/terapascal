use crate::func::ffi::FfiInvoker;
use crate::ir;
use crate::marshal::MarshalResult;
use crate::marshal::Marshaller;
use crate::result::ExecError;
use crate::ExecResult;
use crate::FunctionInfo;
use crate::Vm;
use ir::generic::instantiate_generic;
use ir::generic::instantiate_sig;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_common::SharedStringKey;

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
    ) -> MarshalResult<Self> {
        let invoker = marshaller.build_ffi_invoker(&func_ref)?;

        let func_name =  format!("{}::{}", func_ref.src, func_ref.symbol);
        let debug_name = Self::make_debug_name(&func_name, &func_ref.sig.param_tys, marshaller.metadata());

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
            let arg_size = marshaller.get_native_type(arg_ty)?.size();
            args_size += arg_size;
        }

        let return_size = match self.return_ty() {
            ir::Type::Nothing => 0,
            return_ty => marshaller.get_native_type(return_ty)?.size(),
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

struct RuntimeFuncBuilder<'a> {
    vm: &'a Vm,

    local_stack: ir::LocalStack,

    debug_stack: Vec<Span>,

    next_label: ir::Label,

    body: ir::InstructionList,
}

impl<'a> RuntimeFuncBuilder<'a> {
    pub fn new(vm: &'a Vm) -> Self {
        Self {
            vm,
            local_stack: ir::LocalStack::new(),
            debug_stack: Vec::new(),
            next_label: ir::Label(ir::EXIT_LABEL.0 + 1),
            body: ir::InstructionList::new()
        }
    }

    pub fn finish(mut self) -> ir::InstructionList {
        let local_count = self.local_stack.local_slot_count();
        let mut init_instructions = Vec::with_capacity(local_count);

        for (local_id, ty) in self.local_stack.finish() {
            init_instructions.push(ir::Instruction::LocalAlloc(local_id, ty));
        }

        self.body.splice(0..0, init_instructions);
        self.body
    }
}

impl<'a> ir::InstructionBuilder for RuntimeFuncBuilder<'a> {
    fn emit(&mut self, instruction: ir::Instruction) {
        let source = self.debug_stack.last().cloned();
        self.body.push(instruction, source);
    }

    fn metadata(&self) -> &impl ir::MetadataSource {
        self.vm.marshaller().metadata()
    }

    fn local_stack(&self) -> &ir::LocalStack {
        &self.local_stack
    }

    fn local_stack_mut(&mut self) -> &mut ir::LocalStack {
        &mut self.local_stack
    }

    fn is_debug(&self) -> bool {
        true
    }

    fn next_label(&mut self) -> ir::Label {
        let next = self.next_label;
        self.next_label.0 += 1;
        next
    }

    fn push_source(&mut self, ctx: Span) {
        self.debug_stack.push(ctx);
    }

    fn pop_source(&mut self) {
        self.debug_stack.pop();
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FuncInstanceKey {
    pub id: ir::FunctionID,
    pub args: Vec<ir::Type>
}

impl FuncInstanceKey {
    pub fn new(id: ir::FunctionID) -> Self {
        Self {
            id,
            args: Vec::new(),
        }
    }

    pub fn with_args(self, args: impl IntoIterator<Item=ir::Type>) -> Self {
        Self {
            id: self.id,
            args: args.into_iter().collect(),
        }
    }
}

pub fn instantiate_func(
    vm: &mut Vm,
    key: FuncInstanceKey,
) -> ExecResult<FunctionInfo> {
    if let Some(func_info) = vm.functions.get(&key).cloned() {
        return Ok(func_info);
    }

    let generic_func = vm.functions
        .get(&FuncInstanceKey::new(key.id))
        .ok_or_else(|| {
            let msg = format!("missing generic function definition: {}", key.id);
            ExecError::illegal_state(msg)
        })?;

    let generic_def = match generic_func.func.as_ref() {
        Function::IR(func) => &func.def,
        _ => {
            let msg = "definition of generic function must not be external";
            return Err(ExecError::illegal_state(msg))
        }
    };

    if key.args.len() != generic_def.type_params.len() {
        let msg = format!(
            "incorrect number of type parameters for function {} (invocation has {}, expected: {})",
            generic_func.name,
            key.args.len(),
            generic_def.type_params.len(),
        );
        return Err(ExecError::illegal_state(msg))
    }

    let mut types = HashMap::new();
    for (param, arg) in key.args.iter().zip(generic_def.type_params.iter()) {
        if types.insert(SharedStringKey(arg.clone()), param.clone()).is_some() {
            return Err(ExecError::illegal_state("invalid function def: type param names are not unique"));
        }
    }

    let type_args_formatted = format_type_arg_list(&generic_def.type_params, &types, vm.metadata());

    if vm.opts.trace_generics {
        eprintln!(
            "[vm] instantiating function {} with arguments {}",
            generic_func.name,
            type_args_formatted,
        );
    }

    let mut builder = RuntimeFuncBuilder::new(vm);
    let sig = instantiate_sig(&generic_def.sig, &types);

    instantiate_generic(&generic_def, &types, &mut builder);

    let def = ir::FunctionDef {
        body: builder.finish(),
        debug_name: generic_def.debug_name.clone(),
        type_params: Vec::new(),
        sig,
    };

    let func_name = format!("{} {}", generic_func.name, type_args_formatted);

    let func_info = FunctionInfo {
        name: Rc::new(func_name.clone()),
        invoker: None,
        func: Rc::new(Function::IR(IRFunction {
            debug_name: func_name,
            def,
        }))
    };

    vm.functions.insert(key.clone(), func_info.clone());

    Ok(func_info)
}

fn format_type_arg_list(
    params: &[Arc<String>],
    types: &HashMap<SharedStringKey, ir::Type>,
    formatter: &impl ir::IRFormatter,
) -> String {
    let mut result = String::from("[");

    for (i, param) in params.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }

        result.push_str(param.as_str());
        result.push_str("=");
        result.push_str(&types[param].to_pretty_string(formatter));
    }

    result.push_str("]");

    result
}