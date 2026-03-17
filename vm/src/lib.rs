mod builtin;
mod diag;
mod dyn_value;
mod func;
mod heap;
mod marshal;
mod ptr;
pub mod result;
mod rtti_map;
mod stack;

#[cfg(test)]
mod test;

pub use self::dyn_value::*;
pub use self::ptr::Pointer;

use crate::diag::DiagnosticOutput;
use crate::diag::DiagnosticWorker;
use crate::func::BuiltinFn;
use crate::func::BuiltinFunction;
use crate::func::Function;
use crate::heap::NativeHeap;
use crate::heap::NativeHeapError;
use crate::marshal::Marshaller;
use crate::result::ExecError;
use crate::result::ExecResult;
use crate::rtti_map::RttiMap;
use crate::stack::StackFrame;
use crate::stack::StackTrace;
use crate::stack::StackTraceFrame;
use ir::IRFormatter as _;
use smallvec::SmallVec;
use std::collections::hash_map::Entry;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::iter;
use std::ops::BitAnd;
use std::ops::BitOr;
use std::ops::BitXor;
use std::rc::Rc;
use terapascal_ir as ir;
use terapascal_ir::builtin::string_def;
use terapascal_ir::MetadataSource as _;

#[derive(Debug)]
pub struct Vm {
    metadata: Rc<ir::Metadata>,
    stack: Vec<StackFrame>,
    globals: HashMap<ir::GlobalRef, GlobalValue>,

    native_heap: NativeHeap,

    marshaller: Rc<Marshaller>,

    opts: ExecOpts,

    functions: BTreeMap<ir::FunctionID, FunctionInfo>,

    diag_worker: Option<DiagnosticWorker>,

    // cache of RTTI info by the names/IDs used to look them up from user code
    typeinfo_map: RttiMap<ir::Type>,
    funcinfo_map: RttiMap<ir::FunctionID>,

    // all methods with a corresponding MethodInfo object - the impl pointer of each
    // MethodInfo is an index into this list
    runtime_methods: Vec<ir::MethodInfo>,
}

impl Vm {
    pub fn new(opts: ExecOpts) -> Self {
        let globals = HashMap::new();

        let mut marshaller = Marshaller::new();

        let builtin_structs: BTreeMap<_, _> = [
            (ir::STRING_ID, string_def()),
        ].into_iter().collect();
        
        let mut metadata = ir::MetadataBuilder::new(); 
        
        for (id, struct_def) in &builtin_structs {
            metadata.reserve_type(*id);
            match &struct_def.identity {
                ir::StructIdentity::Class(name) => {
                    metadata.declare_type(*id, name)
                }
                ir::StructIdentity::Record(name) => {
                    metadata.declare_type(*id, name)
                }
                _ => {}
            }

            metadata.define_struct(*id, struct_def.clone());
            marshaller.add_struct(*id, struct_def, metadata.metadata())
                .expect("builtin type definition raised a marshalling error");
        }

        let metadata = Rc::new(metadata.build());
        let marshaller = Rc::new(marshaller);
        
        let native_heap = NativeHeap::new(metadata.clone(), marshaller.clone(), opts.trace_heap);

        let diag_worker = match opts.diag_port {
            0 => None,
            port => DiagnosticWorker::new(port),
        };

        Self {
            metadata,
            globals,
            stack: Vec::new(),

            native_heap,

            marshaller,

            opts,

            functions: BTreeMap::new(),

            diag_worker,

            typeinfo_map: RttiMap::new(),
            funcinfo_map: RttiMap::new(),

            runtime_methods: Vec::new(),
        }
    }

    fn add_stack_trace(&self, err: ExecError) -> ExecError {
        match err {
            err @ ExecError::WithStackTrace { .. } => err,
            err => ExecError::WithStackTrace {
                err: Box::new(err),
                stack_trace: self.stack_trace(),
            },
        }
    }

    pub fn stack_trace(&self) -> StackTrace {
        let frames = self
            .stack
            .iter()
            .rev()
            .map(|s| {
                let frame = StackTraceFrame::new(s.name().to_string());
                let Some(span) = s.debug_location() else {
                    return frame;
                };
                frame.with_span(span.into_owned())
            });

        StackTrace::new(frames)
    }

    pub fn stack_trace_formatted(&self) -> String {
        self.stack_trace()
            .into_iter()
            .map(|frame| format!("\t at {}", frame))
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn marshaller(&self) -> &Marshaller {
        &self.marshaller
    }

    pub fn default_struct(&self, id: ir::TypeDefID) -> ExecResult<StructValue> {
        let struct_def = self.metadata.get_struct_def(id).cloned().ok_or_else(|| {
            let msg = format!("missing struct definition in metadata: {}", id);
            ExecError::illegal_state(msg)
        })?;

        let mut fields = Vec::with_capacity(struct_def.fields.len());
        for (&id, field) in &struct_def.fields {
            // include padding of -1s for non-contiguous IDs
            if id.0 >= fields.len() {
                fields.resize(id.0 + 1, DynValue::I32(-1));
            }
            fields[id.0] = self.default_val(&field.ty)?;
        }

        let struct_val = StructValue {
            type_id: id,
            fields,
        };

        Ok(struct_val)
    }

    pub fn default_val(&self, ty: &ir::Type) -> ExecResult<DynValue> {
        let val = match ty {
            ir::Type::I8 => DynValue::I8(i8::MIN),
            ir::Type::U8 => DynValue::U8(u8::MAX),
            ir::Type::I16 => DynValue::I16(i16::MIN),
            ir::Type::U16 => DynValue::U16(u16::MAX),
            ir::Type::I32 => DynValue::I32(i32::MIN),
            ir::Type::U32 => DynValue::U32(u32::MAX),
            ir::Type::I64 => DynValue::I64(i64::MIN),
            ir::Type::U64 => DynValue::U64(u64::MAX),
            ir::Type::ISize => DynValue::ISize(isize::MIN),
            ir::Type::USize => DynValue::USize(usize::MAX),

            ir::Type::Bool => DynValue::Bool(false),
            ir::Type::F32 => DynValue::F32(f32::NAN),
            ir::Type::F64 => DynValue::F64(f64::NAN),

            ir::Type::Struct(id) => DynValue::from(self.default_struct(*id)?),

            ir::Type::Flags(repr_id, ..) => DynValue::from(self.default_struct(*repr_id)?),

            ir::Type::Object(..) | ir::Type::WeakObject(..) => {
                DynValue::Pointer(Pointer::nil(ir::Type::Nothing))
            },

            ir::Type::Pointer(target) | ir::Type::TempRef(target) => {
                DynValue::Pointer(Pointer::nil((**target).clone()))
            },

            ir::Type::Array { element, dim } => {
                let mut elements = Vec::with_capacity(*dim);
                let el = self.default_val(element.as_ref())?;

                for _ in 0..*dim {
                    elements.push(el.clone());
                }

                DynValue::Array(Box::new(ArrayValue {
                    element_type: (**element).clone(),
                    elements,
                }))
            },

            ir::Type::Variant(id) => {
                let default_tag = 0;
                let cases = &self
                    .metadata
                    .get_variant_def(*id)
                    .ok_or_else(|| {
                        ExecError::illegal_state(format!("missing variant definition {}", *id))
                    })?
                    .cases;
                let case_ty = &cases
                    .get(default_tag as usize)
                    .ok_or_else(|| {
                        ExecError::illegal_state(format!(
                            "missing default case definition for variant {}",
                            *id
                        ))
                    })?
                    .ty;

                let default_val = match case_ty {
                    Some(case_ty) => self.default_val(case_ty)?,
                    None => DynValue::Pointer(Pointer::nil(ir::Type::Nothing)),
                };

                DynValue::Variant(Box::new(VariantValue {
                    type_id: *id,
                    tag: Box::new(DynValue::I32(default_tag)),
                    data: Box::new(default_val),
                }))
            },

            ir::Type::Function(..) => DynValue::Function(ir::FunctionID(usize::MAX)),

            _ => {
                let pretty_type = self.metadata.pretty_ty_name(ty);
                let msg = format!("can't initialize default value of type {pretty_type}");
                return Err(ExecError::illegal_state(msg));
            },
        };

        Ok(val)
    }

    pub fn load_indirect(&self, ptr: &Pointer) -> ExecResult<DynValue> {
        let val = self.native_heap.load(ptr)?;

        Ok(val)
    }

    /// dereference a pointer and set the value it points to
    pub fn store_indirect(&mut self, ptr: &Pointer, val: DynValue) -> ExecResult<()> {
        self.native_heap.store(ptr, val)?;

        Ok(())
    }

    pub fn store_result(&mut self, val: DynValue) -> ExecResult<()> {
        let current_frame = self.current_frame_mut()?;
        let local_ptr = current_frame
            .get_result_ptr()
            .map_err(|err| self.add_stack_trace(err.into()))?;

        self.marshaller.marshal_at(&val, &local_ptr)?;

        Ok(())
    }

    pub fn store_arg(&mut self, id: ir::ArgID, val: DynValue) -> ExecResult<()> {
        let current_frame = self.current_frame_mut()?;
        let local_ptr = current_frame
            .get_arg_ptr(id)
            .map_err(|err| self.add_stack_trace(err.into()))?;

        self.marshaller.marshal_at(&val, &local_ptr)?;

        Ok(())
    }

    pub fn store_local(&mut self, id: ir::LocalID, val: DynValue) -> ExecResult<()> {
        let current_frame = self.current_frame_mut()?;
        let local_ptr = current_frame
            .get_local_ptr(id)
            .map_err(|err| self.add_stack_trace(err.into()))?;

        self.marshaller.marshal_at(&val, &local_ptr)?;

        Ok(())
    }

    pub fn load_result(&self) -> ExecResult<DynValue> {
        let current_frame = self.current_frame()?;
        let local_ptr = current_frame
            .get_result_ptr()
            .map_err(|err| self.add_stack_trace(err.into()))?;

        if local_ptr.addr == 0 {
            return Err(ExecError::NativeHeapError(NativeHeapError::NullPointerDeref));
        }

        let value = self.marshaller.unmarshal_at(&local_ptr)?;

        Ok(value)
    }

    pub fn load_arg(&self, id: ir::ArgID) -> ExecResult<DynValue> {
        let current_frame = self.current_frame()?;
        let local_ptr = current_frame
            .get_arg_ptr(id)
            .map_err(|err| self.add_stack_trace(err.into()))?;

        if local_ptr.addr == 0 {
            return Err(ExecError::NativeHeapError(NativeHeapError::NullPointerDeref));
        }

        let value = self.marshaller.unmarshal_at(&local_ptr)?;

        Ok(value)
    }

    pub fn load_local(&self, id: ir::LocalID) -> ExecResult<DynValue> {
        let current_frame = self.current_frame()?;
        let local_ptr = current_frame
            .get_local_ptr(id)
            .map_err(|err| self.add_stack_trace(err.into()))?;

        if local_ptr.addr == 0 {
            return Err(ExecError::NativeHeapError(NativeHeapError::NullPointerDeref));
        }

        let value = self.marshaller.unmarshal_at(&local_ptr)?;

        Ok(value)
    }

    pub fn store(&mut self, at: &ir::Ref, val: DynValue) -> ExecResult<()> {
        match at {
            ir::Ref::Discard => {
                // do nothing with this value
                Ok(())
            },

            ir::Ref::Result => self.store_result(val),
            ir::Ref::Arg(id) => self.store_arg(*id, val),
            ir::Ref::Local(id) => self.store_local(*id, val),

            ir::Ref::Global(name) => {
                let Some(global) = self.globals.get_mut(name) else {
                    return Err(ExecError::illegal_state(format!("global `{name}` is not allocated")));
                };

                match global {
                    GlobalValue::Variable { value, .. } => {
                        let byte_count = self.marshaller.marshal(&val, value.as_mut())?;
                        assert_eq!(byte_count, value.len());
                    },

                    GlobalValue::Function(..) => {
                        let msg = "global function value cannot be assigned to";
                        return Err(ExecError::illegal_state(msg));
                    },

                    GlobalValue::StaticTagArray(..) => {
                        let msg = "global tag array value cannot be assigned to";
                        return Err(ExecError::illegal_state(msg));
                    },
                }

                Ok(())
            },

            ir::Ref::Deref(inner) => match self.evaluate(inner)? {
                DynValue::Pointer(ptr) => {
                    self.store_indirect(&ptr, val)?;

                    Ok(())
                },

                x => {
                    let msg = format!("can't dereference non-pointer val with value {:?}", x);
                    Err(ExecError::illegal_state(msg))
                },
            },

            ir::Ref::Field(..) => {
                Err(ExecError::illegal_state("field pointer cannot be assigned to"))
            }

            ir::Ref::Element(..) => {
                Err(ExecError::illegal_state("element pointer cannot be assigned to"))
            }

            ir::Ref::VariantTag(..) => {
                Err(ExecError::illegal_state("variant tag pointer cannot be assigned to"))
            }

            ir::Ref::VariantData(..) => {
                Err(ExecError::illegal_state("variant data pointer cannot be assigned to"))
            }
        }
    }

    pub fn load(&self, at: &ir::Ref) -> ExecResult<DynValue> {
        match at {
            ir::Ref::Discard => {
                let msg = "can't read value from discard ref";
                Err(ExecError::illegal_state(msg))
            },
            
            ir::Ref::Result => self.load_result(),
            ir::Ref::Arg(id) => self.load_arg(*id),
            ir::Ref::Local(id) => self.load_local(*id),

            ir::Ref::Global(name) => match self.globals.get(name) {
                Some(GlobalValue::Function(id)) => Ok(DynValue::Function(*id)),

                Some(GlobalValue::Variable { value, ty }) => {
                    let val = self.marshaller.unmarshal(value, ty)?;
                    Ok(val.value)
                },

                Some(GlobalValue::StaticTagArray(loc)) => Ok(DynValue::from(loc.clone())),

                None => {
                    let ref_name = at.to_pretty_string(self.metadata.as_ref());
                    let msg = format!("global `{ref_name}` is not allocated");
                    Err(ExecError::illegal_state(msg))
                },
            },

            ir::Ref::Deref(inner) => match self.evaluate(inner)? {
                DynValue::Pointer(ptr) => {
                    if ptr.ty == ir::Type::Nothing {
                        let ref_name = at.to_pretty_string(self.metadata.as_ref());
                        let msg = format!("can't deref untyped pointer: {ref_name}");
                        return Err(ExecError::illegal_state(msg));
                    }
                    
                    self.load_indirect(&ptr)
                },

                x => {
                    let ref_name = at.to_pretty_string(self.metadata.as_ref());
                    let msg = format!("can't dereference value: {ref_name} ({})", x.value_type_category());
                    Err(ExecError::illegal_state(msg))
                },
            },

            ir::Ref::Field(field_ref) => {
                let field_ptr = self.field_ptr(&field_ref.instance, &field_ref.instance_type, field_ref.field)?;
                Ok(DynValue::Pointer(field_ptr))
            }

            ir::Ref::Element(el_ref) => {
                let element_ptr = self.element_ptr(&el_ref.instance, &el_ref.instance_type, &el_ref.index)?;
                Ok(DynValue::Pointer(element_ptr))
            }

            ir::Ref::VariantTag(tag_ref) => {
                let tag_ptr = self.variant_tag_ptr(&tag_ref.instance)?;
                Ok(DynValue::Pointer(tag_ptr))
            }

            ir::Ref::VariantData(data_ref) => {
                let data_ptr = self.variant_data_ptr(
                    &data_ref.instance,
                    &data_ref.instance_type,
                    data_ref.case_index,
                )?;
                Ok(DynValue::Pointer(data_ptr))
            }
        }
    }

    pub fn evaluate(&self, val: &ir::Value) -> ExecResult<DynValue> {
        match val {
            ir::Value::LiteralU8(i) => Ok(DynValue::U8(*i)),
            ir::Value::LiteralI8(i) => Ok(DynValue::I8(*i)),
            ir::Value::LiteralI16(i) => Ok(DynValue::I16(*i)),
            ir::Value::LiteralU16(i) => Ok(DynValue::U16(*i)),
            ir::Value::LiteralI32(i) => Ok(DynValue::I32(*i)),
            ir::Value::LiteralU32(i) => Ok(DynValue::U32(*i)),
            ir::Value::LiteralI64(i) => Ok(DynValue::I64(*i)),
            ir::Value::LiteralU64(i) => Ok(DynValue::U64(*i)),
            ir::Value::LiteralISize(i) => Ok(DynValue::ISize(*i)),
            ir::Value::LiteralUSize(i) => Ok(DynValue::USize(*i)),
            ir::Value::LiteralF32(f) => Ok(DynValue::F32(*f)),
            ir::Value::LiteralF64(f) => Ok(DynValue::F64(*f)),
            ir::Value::LiteralBool(b) => Ok(DynValue::Bool(*b)),
            ir::Value::LiteralNil => Ok(DynValue::Pointer(Pointer::nil(ir::Type::Nothing))),

            ir::Value::Ref(r) => {
                let ref_val = self.load(r)?;
                Ok(ref_val)
            },

            ir::Value::SizeOf(ty) => {
                let marshal_ty = self.marshaller.get_ty(ty)?;
                let size = cast::i32(marshal_ty.size()).map_err(|_| {
                    ExecError::illegal_state(format!(
                        "type has illegal size: {}",
                        marshal_ty.size()
                    ))
                })?;

                Ok(DynValue::I32(size))
            },

            ir::Value::Default(ty) => {
                self.default_val(ty)
            }
        }
    }

    fn push_stack(&mut self, name: Rc<String>, stack_size: usize) {
        let stack_frame = StackFrame::new(name, self.marshaller.clone(), stack_size);
        self.stack.push(stack_frame);
    }

    fn pop_stack(&mut self) -> ExecResult<()> {
        let popped = self
            .stack
            .pop()
            .ok_or_else(|| ExecError::illegal_state("popped stack with no stackframes"))?;
        popped.check_sentinel()?;
        Ok(())
    }

    fn current_frame(&self) -> ExecResult<&StackFrame> {
        self.stack
            .last()
            .ok_or_else(|| ExecError::illegal_state("called current_frame without no stackframes"))
    }

    fn current_frame_mut(&mut self) -> ExecResult<&mut StackFrame> {
        match self.stack.last_mut() {
            Some(frame) => Ok(frame),
            None => Err(ExecError::illegal_state(
                "called current_frame without no stackframes",
            )),
        }
    }

    fn vcall_lookup(
        &self,
        self_val: &DynValue,
        iface_id: ir::InterfaceID,
        method: ir::MethodID,
    ) -> ExecResult<ir::FunctionID> {
        let self_ptr = self_val.as_pointer().ok_or_else(|| {
            let msg = "expected target of vcall to be a pointer";
            ExecError::illegal_state(msg)
        })?;
        
        let instance_ty = self.load_object_header(self_ptr)?.id.to_type();

        self.metadata
            .find_virtual_impl(&instance_ty, iface_id, method)
            .ok_or_else(|| {
                let mut err = "virtual call ".to_string();

                let iface_ty = ir::Type::Object(ir::ObjectID::Interface(iface_id));
                let _ = self.metadata.format_type(&iface_ty, &mut err);
                err.push('.');
                let _ = self.metadata.format_method(iface_id, method, &mut err);
                err.push_str(" missing implementation for ");
                let _ = self.metadata.format_type(&instance_ty, &mut err);

                ExecError::illegal_state(format!("{}", err))
            })
    }

    fn call_and_store(
        &mut self,
        id: ir::FunctionID,
        args: &[DynValue],
        out: Option<&ir::Ref>,
    ) -> ExecResult<()> {
        let result_val = self.call(id, args)?;

        match (result_val, out) {
            (Some(v), Some(out_at)) => {
                self.store(&out_at, v)?;
            },

            (None, Some(_)) => {
                let msg = "called function which has no return type in a context where a return value was expected";
                return Err(ExecError::illegal_state(msg));
            },

            // ok, no output expected, ignore result if there is one
            (_, None) => {},
        }

        Ok(())
    }

    pub fn call(&mut self, id: ir::FunctionID, args: &[DynValue]) -> ExecResult<Option<DynValue>> {
        let func_info = self
            .functions
            .get(&id)
            .ok_or_else(|| {
                let msg = format!("missing function: {id}");
                ExecError::illegal_state(msg)
            })?
            .clone();

        let func = &func_info.func;
        let stack_size = func.stack_alloc_size(self.marshaller())?;

        self.push_stack(func_info.name.clone(), stack_size);

        // store empty result if there is a result value
        let return_ty = func.return_ty();

        if *return_ty != ir::Type::Nothing {
            let result_val = self.default_val(return_ty)?;
            self.current_frame_mut()?.declare_result(return_ty.clone(), &result_val)?;
        }

        if args.len() != func.param_tys().len() {
            let msg = format!(
                "arguments provided for call to {} are invalid (expected {} args, got {})",
                func_info.name,
                func.param_tys().len(),
                args.len()
            );
            return Err(ExecError::illegal_state(msg));
        }

        for (arg_num, (arg_val, param_ty)) in args.iter().zip(func.param_tys()).enumerate() {
            let arg_id = self
                .current_frame_mut()?
                .declare_arg(param_ty.clone(), arg_val)?;

            assert_eq!(ir::ArgID(arg_num), arg_id);
        }

        func.invoke(self)?;

        let result_val = match return_ty {
            ir::Type::Nothing => None,
            
            _ => {
                let return_val = self.evaluate(&ir::Ref::Result.value())?;
                Some(return_val)
            },
        };

        self.pop_stack()?;

        Ok(result_val)
    }

    fn invoke_dtor(&mut self, val: &DynValue, ty: &ir::Type) -> ExecResult<()> {
        let dtor_func_id = self.metadata
            .get_type_info(ty)
            .and_then(|runtime_type| runtime_type.dtor);

        // eprintln!("trying to invoke dtor for {}... {:?}, {:?}: {:?}", ty, ty.def_id(), ty.rc_resource_def_id(), dtor_func_id);

        if let Some(func_id) = dtor_func_id {
            let func_desc = self
                .metadata
                .func_desc(func_id)
                .unwrap_or_else(|| func_id.to_string());

            if self.opts.trace_rc {
                eprintln!("[rc] invoking dtor {}", func_desc);
            }

            self.call_and_store(func_id, &[val.clone()], None)?;
        } else if self.opts.trace_rc {
            eprintln!("[rc] no dtor for {}", self.metadata.pretty_ty_name(ty));
        }

        Ok(())
    }

    // load a pointer, expected to point to an RC struct
    // guarantees that the struct value returned has some value for its rc field
    fn load_dyn_array_ptr(&self, ptr: &Pointer) -> ExecResult<(ArrayValue, ObjectHeader)> {
        let object_value = self.marshaller.unmarshal_object_at(ptr)?;

        let elements_array = match object_value.value {
            DynValue::Array(array_val) => *array_val,

            _ => {
                return Err(ExecError::illegal_state(format!(
                    "loaded val was not an array, found: {}",
                    object_value.header.id.to_type().to_pretty_string(self.metadata.as_ref())
                )));
            },
        };

        Ok((elements_array, object_value.header))
    }
    
    // TODO: move to NativeHeap
    fn load_object_header(&self, ptr: &Pointer) -> ExecResult<ObjectHeader> {
        if ptr.is_null() {
            return Err(ExecError::NativeHeapError(NativeHeapError::NullPointerDeref));
        }
        
        let rc = self.marshaller.unmarshal_object_header(unsafe {
            ptr.as_slice(Marshaller::object_header_size())
        })?;

        Ok(rc.value)
    }

    // TODO: move to NativeHeap
    fn store_object_header(&self, header: &ObjectHeader, ptr: &Pointer) -> ExecResult<()> {
        if ptr.is_null() {
            return Err(ExecError::NativeHeapError(NativeHeapError::NullPointerDeref));
        }
        
        self.marshaller.marshal_object_header(header, unsafe {
            ptr.as_slice_mut(Marshaller::object_header_size())
        })?;

        Ok(())
    }

    // load a pointer, expected to point to an RC class object
    fn load_class_object(&self, ptr: &Pointer) -> ExecResult<(Box<StructValue>, ObjectHeader)> {
        let object_val = self.native_heap.load_object(ptr)?;

        let struct_val = match object_val.value {
            DynValue::Structure(struct_val) => struct_val,

            other => {
                let msg = format!("loaded val was not a structure, found: {:?}", other);
                return Err(ExecError::illegal_state(msg));
            },
        };

        Ok((struct_val, object_val.header))
    }

    fn release_dyn_val(&mut self, val: &DynValue, weak: bool) -> ExecResult<bool> {
        let ptr = val.as_pointer().ok_or_else(|| {
            let msg = format!("released val was not a pointer, found: {:?}", val);
            ExecError::illegal_state(msg)
        })?;
        
        // NULL is a valid release target because we release uninitialized local RC pointers
        // just do nothing
        if ptr.is_null() {
            return Ok(false);
        }
        
        let mut rc = self.load_object_header(ptr)?;

        // release calls are totally ignored for immortal refs
        if rc.strong_count < 0 {
            return Ok(false);
        }

        if weak {
            if rc.weak_count == 0 {
                panic!(
                    "releasing with 0 weak refs remaining @ {} <{}> (+{} weak refs remain)\n{}",
                    ptr.to_pretty_string(&self.metadata),
                    rc.id.to_type().to_pretty_string(self.metadata.as_ref()),
                    rc.weak_count,
                    self.stack_trace_formatted(),
                );
            }

            rc.weak_count -= 1;
        } else {
            if rc.strong_count == 0 {
                panic!(
                    "releasing with 0 strong refs remaining @ {} <{}> (+{} strong refs remain)\n{}",
                    ptr.to_pretty_string(&self.metadata),
                    rc.id.to_type().to_pretty_string(self.metadata.as_ref()),
                    rc.strong_count,
                    self.stack_trace_formatted(),
                );
            }

            // if we just removed the last strong reference, destroy the object, making it a zombie
            // until the last weak ref is removed, if there are any
            if rc.strong_count == 1 {
                if self.opts.trace_rc {
                    eprintln!(
                        "[rc] destroy @ {} <{}> ({}+{} refs will remain)",
                        ptr.to_pretty_string(&self.metadata),
                        rc.id.to_type().to_pretty_string(self.metadata.as_ref()),
                        rc.strong_count - 1,
                        rc.weak_count,
                    );
                }

                self.invoke_dtor(&val, &rc.id.to_type())?;
            }

            rc.strong_count -= 1;
        }

        if self.opts.trace_rc {
            eprintln!(
                "[rc] release @ {} <{}> ({}+{} refs remain)",
                ptr.to_pretty_string(&self.metadata),
                rc.id.to_type().to_pretty_string(self.metadata.as_ref()),
                rc.strong_count,
                rc.weak_count,
            )
        }

        if rc.strong_count == 0 && rc.weak_count == 0 {
            // no more refs, free the object
            if self.opts.trace_rc {
                eprintln!(
                    "[rc] free @ {} <{}>", 
                    ptr.to_pretty_string(&self.metadata), 
                    rc.id.to_type().to_pretty_string(self.metadata.as_ref())
                )
            }

            self.dynfree(ptr)?;

            return Ok(true);
        }

        self.store_object_header(&rc, &ptr)?;

        Ok(false)
    }

    fn retain_dyn_val(&mut self, val: &DynValue, weak: bool) -> ExecResult<()> {
        let ptr = val
            .as_pointer()
            .ok_or_else(|| ExecError::illegal_state(format!("{} cannot be retained", val.value_type_category())))?;

        // retain on null is valid and does nothing
        // it should never normally happen in user code, but with unsafe casting it's
        // possible to create a null RC pointer
        if ptr.is_null() {
            return Ok(());
        }

        let mut rc = self.load_object_header(ptr)?;

        // don't retain immortal refs
        if rc.strong_count < 0 {
            return Ok(());
        }

        if weak {
            rc.weak_count += 1;
        } else {
            if rc.strong_count == 0 {
                panic!(
                    "resurrecting with 0 strong refs @ {} <{}> (+{} weak refs remain)\n{}",
                    ptr.to_pretty_string(&self.metadata),
                    rc.id.to_type().to_pretty_string(self.metadata.as_ref()),
                    rc.strong_count,
                    self.stack_trace_formatted(),
                );
            }

            rc.strong_count += 1;
        }

        if self.opts.trace_rc {
            eprintln!(
                "[rc] retain @ {} <{}> ({}+{} refs)",
                ptr.to_pretty_string(&self.metadata),
                rc.id.to_type().to_pretty_string(self.metadata.as_ref()),
                rc.strong_count,
                rc.weak_count
            );
        }

        self.store_object_header(&rc, ptr)?;

        Ok(())
    }

    fn addr_of_ref(&self, target: &ir::Ref) -> ExecResult<Pointer> {
        match target {
            ir::Ref::Discard => {
                let msg = "can't take address of discard ref";
                Err(ExecError::illegal_state(msg))
            },

            // let int := 1;
            // let intPtr := @int;
            // @(intPtr^) -> address of int behind intPtr
            ir::Ref::Deref(val) => match self.evaluate(val)? {
                DynValue::Pointer(ptr) => Ok(ptr.clone()),

                _ => {
                    let msg = format!("deref of non-pointer value @ {}", val);
                    Err(ExecError::illegal_state(msg))
                },
            },

            ir::Ref::Result => self.current_frame()?.get_result_ptr().map_err(|err| {
                let msg = err.to_string();
                ExecError::illegal_state(msg)
            }),

            ir::Ref::Arg(id) => self.current_frame()?.get_arg_ptr(*id).map_err(|err| {
                let msg = err.to_string();
                ExecError::illegal_state(msg)
            }),

            // let int := 1;
            // @int -> stack address of int val
            ir::Ref::Local(id) => self.current_frame()?.get_local_ptr(*id).map_err(|err| {
                let msg = err.to_string();
                ExecError::illegal_state(msg)
            }),

            ir::Ref::Global(var_ref @ ir::GlobalRef::Variable(..)) => {
                match self.globals.get(var_ref) {
                    Some(GlobalValue::Variable { value, ty }) => {
                        let ptr = Pointer {
                            addr: value.as_ptr() as usize,
                            ty: ty.clone(),
                        };

                        Ok(ptr)
                    },

                    other => {
                        assert!(
                            other.is_none(),
                            "should never store anything other than a variable with a variable key"
                        );

                        let msg = format!(
                            "missing global ref found in address instruction ({})",
                            var_ref
                        );
                        Err(ExecError::illegal_state(msg))
                    },
                }
            },

            ir::Ref::Global(global_ref) => {
                let msg = format!("can't take address of global ref ({})", global_ref);
                Err(ExecError::illegal_state(msg))
            },
            
            ir::Ref::Field(field_ref) => {
                self.field_ptr(&field_ref.instance, &field_ref.instance_type, field_ref.field)
            }

            ir::Ref::Element(element_ref) => {
                self.element_ptr(&element_ref.instance, &element_ref.instance_type, &element_ref.index)
            }

            ir::Ref::VariantTag(tag_ref) => {
                self.variant_tag_ptr(&tag_ref.instance)
            }

            ir::Ref::VariantData(data_ref) => {
                self.variant_data_ptr(
                    &data_ref.instance,
                    &data_ref.instance_type,
                    data_ref.case_index,
                )
            }
        }
    }

    pub fn opts(&self) -> &ExecOpts {
        &self.opts
    }

    pub fn execute(&mut self, instructions: &[ir::Instruction]) -> ExecResult<()> {
        let labels = find_labels(instructions);
        let line_count_width = instructions.len().to_string().len().max(4);

        let mut pc = 0;
        while pc < instructions.len() {
            if self.opts.trace_ir {
                let indent = str::repeat(
                    "    ",
                    self.current_frame()
                        .map(|frame| frame.debug_depth())
                        .unwrap_or(0)
                        .saturating_sub(2),
                );

                let mut instruction_str = String::new();
                self.metadata
                    .format_instruction(&instructions[pc], &mut instruction_str)
                    .unwrap();
                println!(
                    "[vm] {:>width$}| {indent}{}",
                    pc,
                    instruction_str,
                    width = line_count_width
                );
            }

            self.exec_instruction(&instructions[pc], &mut pc, &labels)
                .map_err(|err| self.add_stack_trace(err))?;

            self.update_diagnostics();

            pc += 1;
        }

        Ok(())
    }

    fn exec_instruction(
        &mut self,
        instruction: &ir::Instruction,
        pc: &mut usize,
        labels: &HashMap<ir::Label, LabelLocation>,
    ) -> ExecResult<()> {
        match instruction {
            ir::Instruction::Comment(_) => {
                // noop
            },

            ir::Instruction::DebugPush(ctx) => {
                self.current_frame_mut()?.debug_push(ctx.clone());
            },

            ir::Instruction::DebugPop => {
                self.current_frame_mut()?.debug_pop();
            },

            ir::Instruction::LocalAlloc(id, ty) => {
                self.exec_local_alloc(*id, *pc, ty)?;
            },

            ir::Instruction::NewObject {
                out,
                type_id,
                immortal,
            } => {
                self.exec_new_object(out, *type_id, *immortal)?
            },

            ir::Instruction::NewArray {
                out,
                element_type,
                count,
                immortal,
            } => {
                self.exec_new_array(out, element_type, count, *immortal)?
            },
            
            ir::Instruction::NewBox { out, value_type, immortal } => {
                self.exec_new_box(out, value_type, *immortal)?;
            }

            ir::Instruction::Add(op) => self.exec_add(op)?,

            ir::Instruction::Mul(op) => self.exec_mul(op)?,
            ir::Instruction::IDiv(op) => self.exec_idiv(op)?,
            ir::Instruction::FDiv(op) => self.exec_fdiv(op)?,
            ir::Instruction::Mod(op) => self.exec_mod(op)?,
            ir::Instruction::Sub(op) => self.exec_sub(op)?,
            ir::Instruction::Shl(op) => self.exec_shl(op)?,
            ir::Instruction::Shr(op) => self.exec_shr(op)?,
            ir::Instruction::Eq(op) => self.exec_eq(op)?,
            ir::Instruction::Gt(op) => self.exec_gt(op)?,
            ir::Instruction::Gte(op) => self.exec_gte(op)?,
            ir::Instruction::Lt(op) => self.exec_lt(op)?,
            ir::Instruction::Lte(op) => self.exec_lte(op)?,
            ir::Instruction::Not(op) => self.exec_not(&op)?,
            ir::Instruction::And(op) => self.exec_and(&op)?,
            ir::Instruction::Or(op) => self.exec_or(&op)?,

            ir::Instruction::BitAnd(op) => {
                self.exec_bitwise(op, u64::bitand, ir::Instruction::BitAnd)?
            },
            ir::Instruction::BitOr(op) => {
                self.exec_bitwise(op, u64::bitor, ir::Instruction::BitOr)?
            },
            ir::Instruction::BitXor(op) => {
                self.exec_bitwise(op, u64::bitxor, ir::Instruction::BitXor)?
            },
            ir::Instruction::BitNot(op) => self.exec_bitwise_not(op)?,

            ir::Instruction::Move { out, new_val } => {
                let val = self.evaluate(new_val)?;
                self.store(out, val)?
            },
            ir::Instruction::Call {
                out,
                function,
                args,
            } => self.exec_call(out, function, args)?,

            ir::Instruction::VirtualCall {
                out,
                iface_id,
                method,
                self_arg,
                rest_args,
            } => self.exec_virtual_call(out.as_ref(), *iface_id, *method, &self_arg, rest_args)?,

            ir::Instruction::ClassIs { out, a, class_id } => {
                self.exec_class_is(out, a, class_id)?
            },

            ir::Instruction::AddrOf { out, a } | ir::Instruction::MakeRef { out, a } => {
                let a_ptr = self.addr_of_ref(a)?;
                self.store(out, DynValue::Pointer(a_ptr))?;
            },
            
            ir::Instruction::Length {
                out,
                a,
                of_type,
            } => {
                self.exec_length(out, a, of_type)?;
            }

            ir::Instruction::Label(_) => {
                // noop
            },

            ir::Instruction::Jump { dest } => self.exec_jump(pc, *dest, labels)?,
            ir::Instruction::JumpIf { dest, test } => self.exec_jmpif(pc, &labels, *dest, test)?,

            ir::Instruction::Release { at, weak, released_out } => self.exec_release(at, *weak, released_out)?,
            ir::Instruction::Retain { at, weak } => self.exec_retain(at, *weak)?,

            ir::Instruction::Raise { val } => self.exec_raise(&val)?,

            ir::Instruction::Cast { out, ty, a } => self.exec_cast(out, ty, a)?,
        }

        Ok(())
    }

    fn exec_local_alloc(&mut self, id: ir::LocalID, pc: usize, ty: &ir::Type) -> ExecResult<()> {
        let uninit_val = self.default_val(ty)?;

        let current_frame = self.current_frame_mut()?;
        current_frame
            .declare_local(id, ty.clone(), &uninit_val, pc)
            .map_err(|err| self.add_stack_trace(err.into()))?;

        Ok(())
    }

    fn exec_new_object(
        &mut self,
        out: &ir::Ref,
        struct_id: ir::TypeDefID,
        immortal: bool,
    ) -> ExecResult<()> {
        let struct_val = self.default_struct(struct_id)?;
        let rc_ptr = self.new_object(struct_val, immortal)?;

        self.store(out, DynValue::Pointer(rc_ptr))?;

        Ok(())
    }

    fn exec_new_array(
        &mut self,
        out: &ir::Ref,
        element_type: &ir::Type,
        count: &ir::Value,
        immortal: bool,
    ) -> ExecResult<()> {
        let Some(count) = self.evaluate(count)?.as_i32() else {
            return Err(ExecError::illegal_state("exec_rc_new_array: count value is not i32"));
        };

        let Some(count) = usize::try_from(count).ok() else {
            return Err(ExecError::illegal_state(format!("exec_rc_new_array: count value {count} is a valid size")));
        };

        let default_val = self.default_val(element_type)?;
        let elements = iter::repeat(default_val).take(count).collect();

        let array_ptr = self.new_dyn_array(element_type, elements, immortal)?;
        self.store(out, DynValue::Pointer(array_ptr))?;

        Ok(())
    }

    fn exec_new_box(
        &mut self,
        out: &ir::Ref,
        element_type: &ir::Type,
        immortal: bool,
    ) -> ExecResult<()> {
        let empty_val = self.default_val(element_type)?;
        let box_ptr = self.new_box(element_type, empty_val, immortal)?;

        self.store(out, DynValue::Pointer(box_ptr))?;

        Ok(())
    }

    fn offset_ptr(&self, ptr: Pointer, offset: isize) -> ExecResult<Pointer> {
        let marshal_ty = self.marshaller.get_ty(&ptr.ty)?;
        let ty_size = marshal_ty.size() as isize;

        Ok(Pointer {
            ty: ptr.ty,
            addr: (ptr.addr as isize + offset * ty_size) as usize,
        })
    }

    fn exec_raise(&mut self, val: &ir::Ref) -> ExecResult<()> {
        let msg = self.read_string(&val.clone())?;

        Err(ExecError::Raised { msg })
    }

    fn exec_cast(&mut self, out: &ir::Ref, ty: &ir::Type, a: &ir::Value) -> ExecResult<()> {
        let val = self.evaluate(a)?;
        match val.try_cast(ty) {
            Some(new_val) => {
                self.store(out, new_val)?;
                Ok(())
            },

            None => Err(ExecError::IllegalInstruction(ir::Instruction::Cast {
                out: out.clone(),
                a: a.clone(),
                ty: ty.clone(),
            })),
        }
    }

    pub fn dynfree(&mut self, ptr: &Pointer) -> ExecResult<()> {
        self.native_heap.free(&ptr.clone())?;
        Ok(())
    }

    pub fn dynalloc_init<ValuesIter, Values>(
        &mut self,
        ty: &ir::Type,
        values: Values,
        trace: bool,
    ) -> ExecResult<Pointer>
    where
        Values: IntoIterator<Item = DynValue, IntoIter = ValuesIter>,
        ValuesIter: ExactSizeIterator<Item = DynValue>,
    {
        let values = values.into_iter();
        if values.len() == 0 {
            return Err(ExecError::ZeroLengthAllocation);
        }

        let marshal_ty = self.marshaller.get_ty(&ty)?;
        let marshal_size = marshal_ty.size();

        let alloc_ptr = self.dynalloc(ty, values.len(), trace)?;

        for (i, value) in values.enumerate() {
            let element_offset = i * marshal_size;
            let val_dst = Pointer {
                addr: alloc_ptr.addr + element_offset,
                ty: ty.clone(),
            };
            self.store_indirect(&val_dst, value)?;
        }

        Ok(alloc_ptr)
    }

    pub fn dynalloc(&mut self, ty: &ir::Type, len: usize, trace: bool) -> ExecResult<Pointer> {
        if len == 0 {
            return Err(ExecError::ZeroLengthAllocation);
        }

        let ptr = self.native_heap.alloc(ty.clone(), len, trace)?;

        Ok(ptr)
    }

    fn exec_retain(&mut self, at: &ir::Ref, weak: bool) -> ExecResult<()> {
        let val = self.load(at)?;
        self.retain_dyn_val(&val, weak)?;

        Ok(())
    }

    fn exec_release(&mut self, at: &ir::Ref, weak: bool, released_out: &ir::Ref) -> ExecResult<()> {
        let val = self.load(at)?;

        // to aid with debugging, set freed RC pointers to a recognizable value
        let released = self.release_dyn_val(&val, weak)?;

        if released {
            self.store(at, DynValue::Pointer(Pointer {
                ty: ir::Type::Nothing,
                addr: 0xDEAD,
            }))?;
        }

        self.store(released_out, DynValue::Bool(released))?;

        Ok(())
    }

    fn exec_jmpif(
        &mut self,
        pc: &mut usize,
        labels: &HashMap<ir::Label, LabelLocation>,
        dest: ir::Label,
        cond: &ir::Value,
    ) -> ExecResult<()> {
        let cond_val = self.evaluate(cond)?;
        match cond_val {
            DynValue::Bool(true) => self.exec_jump(pc, dest, labels),
            DynValue::Bool(false) => Ok(()),
            _ => Err(ExecError::illegal_state(
                "JumpIf instruction testing non-boolean value",
            )),
        }
    }

    fn variant_data_ptr(
        &self,
        instance: &ir::Ref,
        instance_type: &ir::Type,
        case_index: usize,
    ) -> ExecResult<Pointer> {
        let variant_id = match instance_type {
            ir::Type::Variant(id) => *id,
            other => {
                let msg = format!(
                    "cannot execute variant data instruction for non-variant type: {}",
                    other
                );
                return Err(ExecError::illegal_state(msg));
            },
        };

        let a_ptr = self.addr_of_ref(instance)?;

        let case_def = self
            .metadata
            .get_variant_def(variant_id)
            .and_then(|def| def.cases.get(case_index))
            .ok_or_else(|| {
                let msg = format!(
                    "missing definition for variant case {}.{}",
                    variant_id, case_index
                );
                ExecError::illegal_state(msg)
            })?;
        let case_ty = case_def.ty.clone().unwrap_or(ir::Type::Nothing);

        // we don't need to actually look this up, the variant data always appears right
        // after the fixed-size tag
        let data_offset = self.marshaller.variant_tag_type().size();

        Ok(Pointer {
            addr: a_ptr.addr + data_offset,
            ty: case_ty,
        })
    }

    fn variant_tag_ptr(&self, instance: &ir::Ref) -> ExecResult<Pointer> {
        // the variant tag is actually just the first member of the variant, so we just need to
        // output a pointer to the variant
        let variant_ptr = self.addr_of_ref(instance)?;

        let tag_type = variant_ptr.ty.as_variant()
            .and_then(|id| self.metadata.get_variant_def(id))
            .map(|def| def.tag_type.clone())
            .unwrap_or(ir::Type::Nothing);

        Ok(variant_ptr.reinterpret(tag_type))
    }
    
    fn find_element_type<'a, 'b: 'a>(&'a self, of_type: &'b ir::Type) -> Option<&'a ir::Type> {
        match of_type {
            ir::Type::Array { element, .. } => Some(element.as_ref()),
            
            ir::Type::Object(ir::ObjectID::Array(element_type)) => {
                Some(element_type.as_ref())
            }

            ir::Type::Object(ir::ObjectID::Box(value_type)) => {
                Some(value_type.as_ref())
            }

            _ => None,
        }
    }

    fn element_ptr(
        &self,
        instance: &ir::Ref,
        instance_type: &ir::Type,
        index: &ir::Value,
    ) -> ExecResult<Pointer> {
        let Some(element_type) = self.find_element_type(instance_type) else {
            return Err(ExecError::illegal_state(&format!("type {} is not an array type", instance_type)));
        };

        let index_value = self
            .evaluate(index)?
            .as_i32()
            .ok_or_else(|| {
                let msg = "element ref has non-integer illegal index value";
                ExecError::illegal_state(msg)
            })?;

        let pointer = match instance_type {
            ir::Type::Object(ir::ObjectID::Array(..)) => {
                let DynValue::Pointer(array_ptr) = self.load(instance)? else {
                    return Err(ExecError::illegal_state("element ref does not refer to a pointer"));
                };

                self.dyn_array_element_pointer(&array_ptr, index_value)?
                    .reinterpret(element_type.clone())
            }

            ir::Type::Object(ir::ObjectID::Box(..)) => {
                let DynValue::Pointer(box_ptr) = self.load(instance)? else {
                    return Err(ExecError::illegal_state("element ref does not refer to a pointer"));
                };

                if index_value != 0 {
                    return Err(ExecError::Raised {
                        msg: format!("invalid box element index: {index_value}")
                    });
                }

                self.box_value_ptr(&box_ptr)?
                    .reinterpret(element_type.clone())
            }

            ir::Type::Array { .. } => {
                let at_ptr = self.addr_of_ref(instance)?;
                let array_ptr = at_ptr.reinterpret(instance_type.clone());

                self.static_array_element_pointer(&array_ptr, index_value)?
                    .reinterpret(element_type.clone())
            }

            other => {
                return Err(ExecError::illegal_state(format!(
                    "element ref instance type {} is not an array or box",
                    other.to_pretty_string(self.metadata.as_ref())
                )));
            }
        };

        Ok(pointer)
    }
    
    fn static_array_element_pointer(&self, array_ptr: &Pointer, index: i32) -> ExecResult<Pointer> {
        let ir::Type::Array { element: element_type, .. } = &array_ptr.ty else {
            return Err(ExecError::illegal_state(&format!(
                "type {} is not an array",
                array_ptr.ty.to_pretty_string(self.metadata.as_ref()),
            )));
        };
        
        let elements_pointer = Pointer {
            addr: array_ptr.addr,
            ty: element_type.as_ref().clone(),
        };
        
        let element_size = self.marshaller.get_ty(&element_type)?.size();

        let index_offset = element_size * usize::try_from(index)
            .map_err(|_| {
                let msg = "element instruction has non-integer illegal index value";
                ExecError::illegal_state(msg)
            })?;

        Ok(elements_pointer.addr_add(index_offset))
    }

    fn dyn_array_element_pointer(&self, array_ptr: &Pointer, index: i32) -> ExecResult<Pointer> {
        let array_header = self.marshaller.unmarshal_dyn_array_header_at(&array_ptr)?;
        
        let element_type = match &array_header.object_header.id {
            ObjectID::Array(t) => t.as_ref().clone(),
            _ => return Err(ExecError::illegal_state("dyn_array_element_pointer: object was not an array")),
        };

        let element_marshal_type = self.marshaller.get_ty(&element_type)?;

        // dynarray access isn't statically bounds checked
        if index < 0 || index >= array_header.len {
            return Err(ExecError::Raised {
                msg: "array index out of bounds".to_string()
            });
        }

        // the elements array starts after the header
        let elements_addr = array_ptr.addr + Marshaller::array_header_size();

        let index_offset = element_marshal_type.size() * (index as usize);
        let element_addr = elements_addr + index_offset;
        
        Ok(Pointer::new(element_addr, element_type))
    }

    #[expect(unused)]
    fn unbox(&mut self, boxed_value: DynValue) -> ExecResult<DynValue> {
        let DynValue::Pointer(box_ptr) = boxed_value else {
            return Err(ExecError::illegal_state("unbox: argument is not a box pointer"));
        };
        
        let header = self.marshaller.unmarshal_object_header(unsafe {
            box_ptr.as_slice(Marshaller::object_header_size())
        })?;
        
        let ObjectID::Box(value_type) = header.value.id else {
            return Err(ExecError::illegal_state("unbox: object is not a box"));
        };

        let value_addr = box_ptr.addr + header.byte_count;
        let value_ptr = Pointer::new(value_addr, (*value_type).clone());
        
        let value = self.marshaller.unmarshal_at(&value_ptr)?;
        Ok(value)
    }

    fn exec_length(
        &mut self,
        out: &ir::Ref,
        a: &ir::Ref,
        of_type: &ir::Type,
    ) -> ExecResult<()> {
        let length = match of_type {
            // assume any object pointer is a dynarray
            ir::Type::Object(..) => {
                let DynValue::Pointer(array_ptr) = self.load(a)? else {
                    return Err(ExecError::illegal_state("argument of element instruction does not refer to a pointer"));
                };

                let header = self.marshaller.unmarshal_dyn_array_header_at(&array_ptr)?;
                header.len
            }
            
            ir::Type::Array { dim, .. } => {
                let Ok(val) = i32::try_from(*dim) else {
                    return Err(ExecError::illegal_state(format!("couldn't convert array size {dim} to a length literal")));  
                };
                
                val
            }
            
            _ => 1,
        };

        self.store(out, DynValue::I32(length))
    }

    fn exec_add(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        let result = match (a_val, b_val) {
            // pointer arithmetic
            (DynValue::Pointer(ptr), DynValue::I32(offset))
            | (DynValue::I32(offset), DynValue::Pointer(ptr)) => {
                let offset_ptr = self.offset_ptr(ptr, offset as isize)?;

                Ok(DynValue::Pointer(offset_ptr))
            },

            // value addition
            (a_val, b_val) => match a_val.try_add(&b_val) {
                Some(result) => Ok(result),
                None => Err(ExecError::IllegalInstruction(ir::Instruction::Add(
                    op.clone(),
                ))),
            },
        }?;

        self.store(&op.out, result)?;
        Ok(())
    }

    fn exec_idiv(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_idiv(&b_val) {
            Some(result) => self.store(&op.out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::IDiv(
                op.clone(),
            ))),
        }
    }

    fn exec_fdiv(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_fdiv(&b_val) {
            Some(result) => self.store(&op.out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::IDiv(
                op.clone(),
            ))),
        }
    }

    fn exec_mod(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_mod(&b_val) {
            Some(result) => self.store(&op.out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Mod(
                op.clone(),
            ))),
        }
    }

    fn exec_mul(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_mul(&b_val) {
            Some(result) => self.store(&op.out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Mul(
                op.clone(),
            ))),
        }
    }

    fn exec_sub(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        let result = match (a_val, b_val) {
            // pointer arithmetic
            (DynValue::Pointer(ptr), DynValue::I32(offset))
            | (DynValue::I32(offset), DynValue::Pointer(ptr)) => {
                let ptr = self.offset_ptr(ptr, -offset as isize)?;

                Ok(DynValue::Pointer(ptr))
            },

            // value addition
            (a_val, b_val) => match a_val.try_sub(&b_val) {
                Some(result) => Ok(result),
                None => Err(ExecError::IllegalInstruction(ir::Instruction::Sub(
                    op.clone(),
                ))),
            },
        }?;

        self.store(&op.out, result)?;
        Ok(())
    }

    fn exec_shl(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_shl(&b_val) {
            Some(result) => self.store(&op.out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Shl(
                op.clone(),
            ))),
        }
    }

    fn exec_shr(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_shr(&b_val) {
            Some(result) => self.store(&op.out, result),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Shr(
                op.clone(),
            ))),
        }
    }

    fn exec_eq(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_eq(&b_val) {
            Some(eq) => self.store(&op.out, DynValue::Bool(eq)),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Eq(
                op.clone(),
            ))),
        }
    }

    fn exec_gt(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_gt(&b_val) {
            Some(gt) => self.store(&op.out, DynValue::Bool(gt)),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Gt(
                op.clone(),
            ))),
        }
    }

    fn exec_gte(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_gte(&b_val) {
            Some(gt) => self.store(&op.out, DynValue::Bool(gt)),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Gte(
                op.clone(),
            ))),
        }
    }

    fn exec_lt(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_lt(&b_val) {
            Some(gt) => self.store(&op.out, DynValue::Bool(gt)),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Lt(
                op.clone(),
            ))),
        }
    }

    fn exec_lte(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;
        let b_val = self.evaluate(&op.b)?;

        match a_val.try_lte(&b_val) {
            Some(gt) => self.store(&op.out, DynValue::Bool(gt)),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Lte(
                op.clone(),
            ))),
        }
    }

    fn exec_not(&mut self, op: &ir::UnaryOpInstruction) -> ExecResult<()> {
        let a_val = self.evaluate(&op.a)?;

        match a_val.try_not() {
            Some(not) => self.store(&op.out, DynValue::Bool(not)),
            None => Err(ExecError::IllegalInstruction(ir::Instruction::Not(
                op.clone(),
            ))),
        }
    }

    fn exec_and(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self
            .evaluate(&op.a)?
            .as_bool()
            .ok_or_else(|| ExecError::IllegalInstruction(ir::Instruction::And(op.clone())))?;

        let b_val = self
            .evaluate(&op.b)?
            .as_bool()
            .ok_or_else(|| ExecError::IllegalInstruction(ir::Instruction::And(op.clone())))?;

        self.store(&op.out, DynValue::Bool(a_val && b_val))?;

        Ok(())
    }

    fn exec_or(&mut self, op: &ir::BinOpInstruction) -> ExecResult<()> {
        let a_val = self
            .evaluate(&op.a)?
            .as_bool()
            .ok_or_else(|| ExecError::IllegalInstruction(ir::Instruction::Or(op.clone())))?;

        let b_val = self
            .evaluate(&op.b)?
            .as_bool()
            .ok_or_else(|| ExecError::IllegalInstruction(ir::Instruction::Or(op.clone())))?;

        self.store(&op.out, DynValue::Bool(a_val || b_val))?;

        Ok(())
    }

    fn store_bitwise_result(
        &mut self,
        out: &ir::Ref,
        arg_cell: &DynValue,
        result: u64,
    ) -> ExecResult<()> {
        self.store(out, match arg_cell {
            DynValue::Pointer(ptr) => DynValue::Pointer(Pointer {
                ty: ptr.ty.clone(),
                addr: result as usize,
            }),

            DynValue::I8(_) => DynValue::I8(result.cast_signed() as i8),
            DynValue::I16(_) => DynValue::I16(result.cast_signed() as i16),
            DynValue::I32(_) => DynValue::I32(result.cast_signed() as i32),
            DynValue::ISize(_) => DynValue::ISize(result.cast_signed() as isize),
            DynValue::I64(_) => DynValue::I64(result.cast_signed()),

            DynValue::U8(_) => DynValue::U8(result as u8),
            DynValue::U16(_) => DynValue::U16(result as u16),
            DynValue::U32(_) => DynValue::U32(result as u32),
            DynValue::USize(_) => DynValue::USize(result as usize),
            DynValue::U64(_) => DynValue::U64(result),
            _ => {
                let msg = format!(
                    "unsupported type for bitwise operation result: {}",
                    arg_cell.value_type_category()
                );

                return Err(ExecError::illegal_state(msg));
            },
        })
    }

    fn exec_bitwise<OpFn, ToInstructionFn>(
        &mut self,
        op: &ir::BinOpInstruction,
        op_fn: OpFn,
        to_instruction: ToInstructionFn,
    ) -> ExecResult<()>
    where
        OpFn: Fn(u64, u64) -> u64,
        ToInstructionFn: Fn(ir::BinOpInstruction) -> ir::Instruction,
    {
        let a_cell = self.evaluate(&op.a)?;

        let a_val = a_cell
            .try_cast(&ir::Type::U64)
            .and_then(|val| val.as_u64())
            .ok_or_else(|| ExecError::IllegalInstruction(to_instruction(op.clone())))?;

        let b_val = self
            .evaluate(&op.b)?
            .try_cast(&ir::Type::U64)
            .and_then(|val| val.as_u64())
            .ok_or_else(|| ExecError::IllegalInstruction(to_instruction(op.clone())))?;

        let result = op_fn(a_val, b_val);
        self.store_bitwise_result(&op.out, &a_cell, result)
    }

    fn exec_bitwise_not(&mut self, op: &ir::UnaryOpInstruction) -> ExecResult<()> {
        let a_cell = self.evaluate(&op.a)?;
        let a_val = a_cell
            .try_cast(&ir::Type::U64)
            .and_then(|val| val.as_u64())
            .ok_or_else(|| ExecError::IllegalInstruction(ir::Instruction::BitNot(op.clone())))?;

        let result = !a_val;
        self.store_bitwise_result(&op.out, &a_cell, result)
    }

    fn exec_call(
        &mut self,
        out: &Option<ir::Ref>,
        function: &ir::Value,
        args: &Vec<ir::Value>,
    ) -> ExecResult<()> {
        let arg_vals: Vec<_> = args
            .iter()
            .map(|arg_val| self.evaluate(arg_val))
            .collect::<ExecResult<_>>()?;

        match self.evaluate(function)? {
            DynValue::Function(function) => {
                self.call_and_store(function, &arg_vals, out.as_ref())?
            },

            _ => {
                let msg = format!("{} does not reference a function", function);
                return Err(ExecError::illegal_state(msg));
            },
        }

        Ok(())
    }

    fn exec_virtual_call(
        &mut self,
        out: Option<&ir::Ref>,
        iface_id: ir::InterfaceID,
        method: ir::MethodID,
        self_arg: &ir::Value,
        rest_args: &[ir::Value],
    ) -> ExecResult<()> {
        let self_val = self.evaluate(&self_arg)?;
        let func = self.vcall_lookup(&self_val, iface_id, method)?;

        let mut arg_vals = vec![self_val];
        for arg_val in rest_args {
            let arg_val = self.evaluate(arg_val)?;
            arg_vals.push(arg_val);
        }

        let func_ref = ir::Ref::Global(ir::GlobalRef::Function(func));
        let func = match self.evaluate(&ir::Value::Ref(func_ref))? {
            DynValue::Function(func) => func,
            unexpected => {
                let msg = format!("invalid function val: {:?}", unexpected);
                return Err(ExecError::illegal_state(msg));
            },
        };

        self.call_and_store(func, &arg_vals, out)?;

        Ok(())
    }

    fn exec_class_is(
        &mut self,
        out: &ir::Ref,
        a: &ir::Value,
        object_id: &ir::ObjectID,
    ) -> ExecResult<()> {
        let a_ptr = self.evaluate(a)?.as_pointer().cloned().ok_or_else(|| {
            let msg = "argument a of ClassIs instruction must evaluate to a pointer";
            ExecError::illegal_state(msg)
        })?;

        if a_ptr.is_null() {
            return self.store(out, DynValue::Bool(false));
        }

        let object_header = self.load_object_header(&a_ptr)?;

        if object_header.strong_count == 0 {
            // zombie references never count as castable to any type 
            // and "is" ops always return false
            self.store(out, DynValue::Bool(false))?;
            return Ok(())
        }
        
        let is = match object_id {
            ir::ObjectID::Any => true,

            ir::ObjectID::Class(class_id) => {
                object_header.id == ObjectID::Class(*class_id)
            },

            ir::ObjectID::Interface(iface_id) => {
                match &object_header.id {
                    // out of all the types a struct might represent, only a class can
                    // implement any interfaces
                    ObjectID::Class(class_id) => {
                        let class_type = class_id.to_class_ptr_type();
                        self.metadata.is_impl(&class_type, *iface_id)
                    },

                    _ => false,
                }
            },

            ir::ObjectID::Closure(func_type_id) => {
                match object_header.id {
                    ObjectID::Class(class_id) => {
                        self.metadata
                            .closures_by_function()
                            .get(&func_type_id)
                            .map(|func_closures| {
                                func_closures.contains(&class_id)
                            })
                            .unwrap_or(false)
                    }
                    
                    _ => false,
                }
            }

            ir::ObjectID::Array(element_type) => {
                object_header.id == ObjectID::Array(element_type.clone())
            }

            ir::ObjectID::Box(value_type) => {
                object_header.id == ObjectID::Box(value_type.clone())
            }
        };

        self.store(out, DynValue::Bool(is))?;

        Ok(())
    }

    fn field_ptr(
        &self,
        instance: &ir::Ref,
        instance_type: &ir::Type,
        field: ir::FieldID,
    ) -> ExecResult<Pointer> {
        match instance_type {
            ir::Type::Flags(repr_id, ..) => {
                self.field_ptr(instance, &ir::Type::Struct(*repr_id), field)
            },

            ir::Type::Struct(..) => {
                let struct_ptr = self.addr_of_ref(instance)?;

                let field_info = self.marshaller.get_field_info(instance_type, field)?;

                Ok(Pointer {
                    ty: field_info.ty.clone(),
                    addr: struct_ptr.addr + field_info.offset,
                })
            },

            // virtual reference, we need to load the actual value behind the pointer to get the
            // concrete type ID. we assume it's a class or closure, the only types to have fields
            ir::Type::Object(ir::ObjectID::Class(..) | ir::ObjectID::Closure(..)) => {
                let DynValue::Pointer(object_ptr) = self.load(instance)? else {
                    return Err(ExecError::illegal_state(format!(
                        "exec_field: expected base value to be an object pointer ({})",
                        instance_type.to_pretty_string(self.metadata.as_ref())
                    )));
                };

                self.object_field_ptr(&object_ptr, field)
            },

            _ => {
                Err(ExecError::illegal_state(format!(
                    "invalid base type referenced in Field instruction: {}.{}",
                    instance_type, field
                )))
            },
        }
    }
    
    fn box_value_ptr(&self, box_ptr: &Pointer) -> ExecResult<Pointer> {
        let header = self.load_object_header(box_ptr)?;

        let ObjectID::Box(value_type) = &header.id else {
            return Err(ExecError::illegal_state(format!(
                "exec_field: loading a box object returned the wrong type of object ({})",
                header.id.to_type().to_pretty_string(self.metadata.as_ref())
            )));
        };

        let addr = box_ptr.addr + Marshaller::object_header_size();
        
        Ok(Pointer::new(addr, value_type.as_ref().clone()))
    }
    
    fn object_field_ptr(&self, object_ptr: &Pointer, field: ir::FieldID) -> ExecResult<Pointer> {
        let header = self.load_object_header(&object_ptr)?;

        let ObjectID::Class(struct_id) = &header.id else {
            return Err(ExecError::illegal_state(format!(
                "exec_field: loading a class object returned the wrong type of object ({})",
                header.id.to_type().to_pretty_string(self.metadata.as_ref())
            )));
        };

        let fields_addr = object_ptr.addr + Marshaller::object_header_size();
        let field_info = self.marshaller.get_field_info(&struct_id.to_class_ptr_type(), field)?;

        Ok(Pointer {
            ty: field_info.ty.clone(),
            addr: fields_addr + field_info.offset,
        })
    }

    fn exec_jump(&mut self,
        pc: &mut usize,
        label: ir::Label,
        labels: &HashMap<ir::Label, LabelLocation>,
    ) -> ExecResult<()> {
        let location = labels
            .get(&label)
            .ok_or_else(|| ExecError::illegal_state(format!("reference to missing label {label}")))?;

        *pc = location.pc_offset;

        Ok(())
    }

    pub fn define_builtin(
        &mut self,
        name: ir::NamePath,
        func: BuiltinFn,
        ret: ir::Type,
        params: Vec<ir::Type>,
    ) {
        // current metadata does not contain this function yet, so we don't have an ID to load it at
        let Some(func_id) = self.metadata.find_function(&name) else {
            return;
        };

        match self.globals.entry(ir::GlobalRef::Function(func_id)) {
            Entry::Occupied(..) => {
                // we already created a definition for this
                // assume this means it's already been loaded
                return;
            }

            Entry::Vacant(entry) => {
                entry.insert(GlobalValue::Function(func_id));
            }
        }

        let func = Function::Builtin(BuiltinFunction {
            func,
            return_ty: ret,
            param_tys: params,
            debug_name: name.to_string(),
        });
        
        let invoker = self.metadata
            .get_function_info(func_id)
            .and_then(|f| f.invoker);

        self.functions.insert(func_id, FunctionInfo {
            func: Rc::new(func),
            name: Rc::new(name.to_string()),
            invoker,
        });
    }

    fn new_object(&mut self, fields: StructValue, immortal: bool) -> ExecResult<Pointer> {
        let fields_type = fields.type_id.to_struct_type();
        let fields_size = self.marshaller.get_ty(&fields_type)?.size();

        let object_id = ObjectID::Class(fields.type_id);
        let object_ptr = self.native_heap.alloc_object(fields_size, object_id.clone(), !immortal)?;

        if !immortal && self.opts.trace_rc {
            eprintln!("[rc] alloc @ {}", object_ptr.to_pretty_string(&self.metadata))
        }

        let header = ObjectHeader::new(object_id, immortal);
        
        let offset = self.marshaller.marshal_object_at(&object_ptr, &ObjectValue {
            header,
            value: DynValue::Structure(Box::new(fields)),
        })?;

        assert_eq!(offset, fields_size + Marshaller::object_header_size());

        Ok(object_ptr)
    }

    fn init_stdlib_globals(&mut self) {
        let system_funcs: Vec<_> = builtin::system_funcs().into_iter().collect();

        for (ident, func, ret, params) in system_funcs {
            let name = ir::NamePath::new(vec!["System".to_string()], ident.to_string());
            self.define_builtin(name, func, ret, params);
        }
    }

    pub fn load_lib(&mut self, lib: &ir::Library) -> ExecResult<()> {
        let mut metadata = (*self.metadata).clone();

        metadata.merge_from(&lib.metadata);

        let mut marshaller = (*self.marshaller).clone();

        for (id, type_def) in lib.metadata().type_defs() {
            let def_result = match type_def {
                ir::TypeDef::Struct(struct_def) => {
                    marshaller
                        .add_struct(id, struct_def, lib.metadata())
                        .map(Some)
                }
                ir::TypeDef::Variant(variant_def) => {
                    marshaller
                        .add_variant(id, variant_def, lib.metadata())
                        .map(Some)
                },
                ir::TypeDef::Function(_func_def) => {
                    // functions don't need special marshalling, we only marshal pointers to them
                    Ok(None)
                },
            };

            def_result.map_err(|err| self.add_stack_trace(err.into()))?;
        }
        
        for (iface_id, _) in lib.metadata.interfaces() {
            marshaller.add_iface(iface_id)
        }

        for (func_id, ir_func) in lib.functions() {
            let func = match ir_func {
                ir::Function::Local(ir_func_def) => {
                    let ir_func = Function::IR(ir_func_def.clone());
                    Some(ir_func)
                },
                
                ir::Function::External(external_ref) 
                if external_ref.src == ir::BUILTIN_SRC => {
                    None
                },

                ir::Function::External(external_ref) => {
                    let ffi_func = Function::new_ffi(external_ref, &mut marshaller, &self.metadata)
                        .map_err(|err| ExecError::WithStackTrace {
                            err: Box::new(ExecError::from(err)),
                            stack_trace: self.stack_trace(),
                        })?;
                    Some(ffi_func)
                }
            };

            let invoker = lib.metadata
                .get_function_info(*func_id)
                .and_then(|f| f.invoker);
            
            if let Some(func) = func {
                self.globals.insert(
                    ir::GlobalRef::Function(*func_id),
                    GlobalValue::Function(*func_id),
                );

                self.functions.insert(*func_id, FunctionInfo {
                    name: Rc::new(match func.debug_name() {
                        Some(name) => name.to_string(),
                        None => format!("{}", func_id),
                    }),
                    func: Rc::new(func),
                    invoker,
                });
            }
        }
        
        for (ty, _) in lib.metadata.type_info() {
            if ty.is_object() {
                marshaller.register_object_type(ty.clone());
            }
        }

        self.metadata = Rc::new(metadata);
        self.marshaller = Rc::new(marshaller);
        self.native_heap
            .set_metadata(self.metadata.clone(), self.marshaller.clone());

        // we need to check again after loading a new lib, which might have declared some IDs for
        // builtin functions that can now be stored as globals
        self.init_stdlib_globals();

        for (id, literal) in lib.metadata().strings() {
            let str_val = self
                .create_string(literal, true)
                .map_err(|err| ExecError::WithStackTrace {
                    err: err.into(),
                    stack_trace: self.stack_trace(),
                })?;

            let str_ref = ir::GlobalRef::StringLiteral(id);

            self.globals.insert(str_ref, GlobalValue::Variable {
                value: self.marshaller.marshal_to_vec(&str_val)?.into_boxed_slice(),
                ty: ir::STRING_ID.to_class_ptr_type(),
            });
        }
        
        let disable_rtti = self.metadata.get_struct_def(ir::TYPEINFO_ID).is_none()
            || self.metadata.get_struct_def(ir::FUNCINFO_ID).is_none()
            || self.metadata.get_struct_def(ir::METHODINFO_ID).is_none();

        if !disable_rtti {
            let tag_counts = lib.metadata
                .all_tags()
                .map(|(loc, tags)| (loc, tags.len()));
            
            // allocate static arrays for runtime tag objects (must exist before RTTI init)
            for (tag_loc, tag_count) in tag_counts {
                let nil_any = DynValue::Pointer(Pointer::nil(ir::ANY_TYPE));
                let elements = iter::repeat(nil_any).take(tag_count).collect();

                let empty_tag_array = self.new_dyn_array(&ir::ANY_TYPE, elements, true)?;

                let array_ref = ir::GlobalRef::StaticTagArray(tag_loc);
                let array_value = GlobalValue::StaticTagArray(empty_tag_array);
                self.globals.insert(array_ref, array_value);
            }

            self.init_rtti(lib)?;
        } else if self.opts.verbose {
            eprintln!("[vm] RTTI is disabled (required types are not loaded)");
        }

        // declare global variables
        for (var_id, var) in lib.metadata.variables() {
            // global variables start zero-initialized
            let marshal_ty = self.marshaller.get_ty(&var.r#type)?;
            let zero_val = vec![0u8; marshal_ty.size()];

            self.globals
                .insert(ir::GlobalRef::Variable(var_id), GlobalValue::Variable {
                    value: zero_val.into_boxed_slice(),
                    ty: var.r#type.clone(),
                });
        }

        // declare (uninitialized) global vars for static closure pointers
        for static_closure in &lib.static_closures {
            let closure_ptr_ref = ir::GlobalRef::StaticClosure(static_closure.id);
            let closure_ptr_ty =
                ir::Type::Object(ir::ObjectID::Closure(static_closure.func_ty_id));

            // we only need to set a null pointer here, init code will set the actual value
            let default_val = self.default_val(&closure_ptr_ty)?;
            let default_val_bytes = self.marshaller.marshal_to_vec(&default_val)?;

            self.globals.insert(closure_ptr_ref, GlobalValue::Variable {
                ty: closure_ptr_ty,
                value: default_val_bytes.into_boxed_slice(),
            });
        }

        let init_stack_size = self
            .marshaller
            .stack_alloc_size(lib.init())
            .map_err(|err| self.add_stack_trace(err.into()))?;

        if !lib.init.is_empty() {
            if self.opts.verbose {
                println!("[vm] entering library init: {}", lib.name);
            }

            self.push_stack(Rc::new("<init>".to_string()), init_stack_size);

            self.execute(&lib.init)?;

            self.pop_stack()?;

            if self.opts.verbose {
                println!("[vm] exiting library init: {}", lib.name);
            }
        }

        Ok(())
    }
    
    fn init_rtti(&mut self,
        lib: &ir::Library) -> ExecResult<()> {
        // create runtime type info objects (TypeInfo and MethodInfo)
        for (ty, runtime_type) in lib.metadata.type_info() {
            let typeinfo_ref = ir::GlobalRef::StaticTypeInfo(Rc::new(ty.clone()));

            let typeinfo_ptr = self.create_typeinfo(ty, runtime_type)?;
            let ptr_bytes = self.marshaller.marshal_to_vec(&typeinfo_ptr)?;

            self.globals
                .insert(typeinfo_ref.clone(), GlobalValue::Variable {
                    value: ptr_bytes.into_boxed_slice(),
                    ty: ir::TYPEINFO_TYPE,
                });

            let runtime_name = runtime_type
                .name
                .as_ref()
                .and_then(|str_id| self.metadata.get_string(*str_id))
                .cloned();

            // let object_id = ObjectID::try_from_type(ty);

            self.typeinfo_map.add(Some(ty.clone()), runtime_name, typeinfo_ref.clone());
        }

        for (func_id, func_decl) in lib.metadata.functions() {
            let runtime_name = func_decl
                .runtime_name
                .as_ref()
                .and_then(|str_id| self.metadata.get_string(*str_id))
                .cloned();

            let funcinfo_ptr = self.create_func_info_object(func_id, func_decl)?;
            let ptr_bytes = self.marshaller.marshal_to_vec(&funcinfo_ptr)?;

            let funcinfo_ref = ir::GlobalRef::StaticFuncInfo(func_id);
            self.globals
                .insert(funcinfo_ref.clone(), GlobalValue::Variable {
                    value: ptr_bytes.into_boxed_slice(),
                    ty: ir::FUNCINFO_TYPE,
                });

            self.funcinfo_map
                .add(Some(func_id), runtime_name, funcinfo_ref);
        }
        
        Ok(())
    }

    fn load_string_lit(&self, id: ir::StringID) -> ExecResult<DynValue> {
        let global_ref = ir::GlobalRef::StringLiteral(id);
        self.load(&ir::Ref::from(global_ref))
    }

    pub fn create_string(&mut self, content: &str, immortal: bool) -> ExecResult<DynValue> {
        let mut chars: Vec<_> = content.chars().map(|c| DynValue::U8(c as u8)).collect();
        let chars_len = cast::i32(chars.len()).map_err(|_| {
            let msg = format!("string length out of range: {}", chars.len());
            ExecError::illegal_state(msg)
        })?;

        // add a null-terminator, not included in the char count
        chars.push(DynValue::U8(0));

        let chars_ptr = self.dynalloc_init(&ir::Type::U8, chars, !immortal)?;

        let mut string_struct = self.default_struct(ir::STRING_ID)?;
        string_struct[ir::STRING_LEN_FIELD] = DynValue::I32(chars_len);
        string_struct[ir::STRING_CHARS_FIELD] = DynValue::Pointer(chars_ptr);

        let str_ptr = self.new_object(string_struct, immortal)?;

        Ok(DynValue::Pointer(str_ptr))
    }

    pub fn read_string_at(&self, str_ptr: &Pointer) -> ExecResult<String> {
        let (str_struct, _) = self.load_class_object(str_ptr)?;
        self.read_string_struct(str_struct.as_ref())
    }

    // reads the string value stored in the string object that `str_ref` is a pointer to
    pub fn read_string(&self, str_ref: &ir::Ref) -> ExecResult<String> {
        let str_ptr_val = self.load(str_ref)?;
        let str_ptr = str_ptr_val
            .as_pointer()
            .ok_or_else(|| {
                let msg = format!("invalid value at {str_ref}, expected a string pointer, got {}", str_ptr_val.value_type_category());
                ExecError::illegal_state(msg)
            })?;

        self.read_string_at(str_ptr)
    }

    fn read_string_struct(&self, str_struct: &StructValue) -> ExecResult<String> {
        let len_val = &str_struct[ir::STRING_LEN_FIELD];
        let len = len_val
            .as_i32()
            .and_then(|len| cast::usize(len).ok())
            .ok_or_else(|| {
                let msg = format!("string length value contained invalid value: {:?}", len_val);
                ExecError::illegal_state(msg)
            })?;

        if len == 0 {
            return Ok(String::new());
        }

        let chars_ptr = str_struct[ir::STRING_CHARS_FIELD]
            .as_pointer()
            .ok_or_else(|| {
                ExecError::illegal_state(format!(
                    "string contained invalid `chars` pointer value: {:?}",
                    str_struct
                ))
            })?;

        let mut chars = Vec::new();
        for i in 0..len {
            let char_ptr = Pointer {
                addr: chars_ptr.addr + i,
                ty: chars_ptr.ty.clone(),
            };

            let char_val = self.load_indirect(&char_ptr)?.as_u8().ok_or_else(|| {
                ExecError::illegal_state(format!("expected string char @ {}", char_ptr))
            })?;

            chars.push(char_val as char);
        }

        Ok(chars.into_iter().collect())
    }

    #[allow(unused)]
    fn create_variant_tag(
        &self,
        variant: &ir::VariantDef,
        case_name: &str,
    ) -> ExecResult<DynValue> {
        let case_index = variant
            .cases
            .iter()
            .position(|case| case.name == case_name)
            .ok_or_else(|| {
                let msg = format!("missing definition of {}.{} case", variant.name, case_name);
                ExecError::illegal_state(msg)
            })?;

        DynValue::USize(case_index)
            .try_cast(&variant.tag_type)
            .ok_or_else(|| {
                let msg = format!(
                    "failed to cast tag value {} for {}.{} case",
                    case_index, variant.name, case_name
                );
                ExecError::illegal_state(msg)
            })
    }

    fn new_box(
        &mut self,
        value_ty: &ir::Type,
        value: DynValue,
        immortal: bool,
    ) -> ExecResult<Pointer> {
        let object_id = ObjectID::Box(Rc::new(value_ty.clone()));
        let header = ObjectHeader::new(object_id.clone(), immortal);

        let immortal = header.is_immortal();

        let value_size = self.marshaller.get_ty(value_ty)?.size();

        let box_ptr = self.native_heap.alloc_object(value_size, object_id, !immortal)?;

        if !immortal && self.opts.trace_rc {
            eprintln!("[rc] alloc @ {}", box_ptr.to_pretty_string(&self.metadata))
        }

        let offset = self.marshaller.marshal_object_at(&box_ptr, &ObjectValue {
            header,
            value,
        })?;

        assert_eq!(offset, value_size + Marshaller::object_header_size());

        Ok(box_ptr)
    }

    fn new_dyn_array(
        &mut self,
        element_ty: &ir::Type,
        elements: Vec<DynValue>,
        immortal: bool,
    ) -> ExecResult<Pointer> {
        let object_id = ObjectID::Array(Rc::new(element_ty.clone()));
        let header = ObjectHeader::new(object_id, immortal);

        self.new_dyn_array_with_header(element_ty, elements, header)
    }

    fn new_dyn_array_with_header(
        &mut self,
        element_ty: &ir::Type,
        elements: Vec<DynValue>,
        header: ObjectHeader,
    ) -> ExecResult<Pointer> {
        let immortal = header.is_immortal();

        let array_len = elements.len();

        let element_size = self.marshaller.get_ty(&element_ty)?.size();
        let data_size = element_size * array_len;

        // we can't allocate a fixed-size object here so allocate bytes and reinterpret the pointer
        let object_id = ObjectID::Array(Rc::new(element_ty.clone()));
        let array_ptr = self.native_heap.alloc_object(data_size, object_id.clone(), !immortal)?;

        if !immortal && self.opts.trace_rc {
            eprintln!("[rc] alloc @ {}", array_ptr.to_pretty_string(&self.metadata))
        }

        let offset = self.marshaller.marshal_object_at(&array_ptr, &ObjectValue {
            header: ObjectHeader::new(object_id, immortal),
            value: DynValue::Array(Box::new(ArrayValue {
                element_type: element_ty.clone(),
                elements,
            })),
        })?;

        assert_eq!(offset, Marshaller::array_header_size() + data_size);

        Ok(array_ptr)
    }

    pub fn read_dynarray(&self, ptr: &Pointer) -> ExecResult<Vec<DynValue>> {
        let (array_struct, _) = self.load_dyn_array_ptr(ptr)?;
        Ok(array_struct.elements)
    }

    fn get_tags_array_ptr(&self, loc: ir::TagLocation) -> Option<Pointer> {
        let tags_global = &ir::GlobalRef::StaticTagArray(loc);

        let Some(GlobalValue::StaticTagArray(ptr)) = self.globals.get(tags_global) else {
            return None;
        };

        Some(ptr.clone())
    }

    fn create_typeinfo(
        &mut self,
        ty: &ir::Type,
        type_info: &ir::TypeInfo,
    ) -> ExecResult<DynValue> {
        let type_name_string = self.load_string_lit(type_info.name.unwrap_or(ir::EMPTY_STRING_ID))?;

        let ty_tags_loc = match ty {
            ir::Type::Object(ir::ObjectID::Interface(iface_id)) => {
                Some(ir::TagLocation::Interface(*iface_id))
            },

            ir::Type::Object(ir::ObjectID::Class(id))
            | ir::Type::Struct(id)
            | ir::Type::Variant(id) => Some(ir::TagLocation::TypeDef(*id)),

            _ => None,
        };

        let ty_tags_array_ptr = ty_tags_loc
            .and_then(|loc| self.get_tags_array_ptr(loc))
            .unwrap_or_else(|| Pointer::nil(ir::Type::Nothing));
        
        let type_flags_repr = self.metadata.find_set_repr_struct(ir::TYPEINFO_FLAGS_BITS)
            .ok_or_else(|| ExecError::illegal_state("missing flags type for TypeFlags"))?;
        let type_flags_val = StructValue::new(type_flags_repr, [
            DynValue::U64(type_info.flags)
        ]);

        let typeinfo_methods_ptr = DynValue::Pointer(
            Pointer::nil(ir::Type::Struct(ir::METHODINFO_ID))
        );

        // allocate and store the typeinfo before populating methods, so we can easily
        // get a real heap pointer to use for the "owner" field
        let typeinfo_struct = StructValue::new(ir::TYPEINFO_ID, [
            // 0: name
            type_name_string,
            // 1: methods
            typeinfo_methods_ptr,
            // 2: tags
            DynValue::Pointer(ty_tags_array_ptr),
            // 3: impl
            DynValue::Pointer(Pointer::nil(ir::Type::Nothing)),
            // 3: flags
            DynValue::from(type_flags_val),
        ]);

        let typeinfo_ptr = self.new_object(typeinfo_struct, true)?;

        // the metadata object has more method info than is stored in the actual MethodInfos
        let method_func_ids: Vec<_> = self
            .metadata
            .get_runtime_methods(ty)
            .map(|method| method.function)
            .collect();

        // these should be the same method in the same order!
        assert_eq!(method_func_ids.len(), type_info.methods.len());

        let mut method_info_ptrs = Vec::new();
        for method_index in 0..method_func_ids.len() {
            let method_info = &type_info.methods[method_index];

            let method_tags_array_ptr = ty_tags_loc
                .and_then(|loc| loc.method_loc(method_index))
                .and_then(|loc| self.get_tags_array_ptr(loc))
                .unwrap_or_else(|| Pointer::nil(ir::Type::Nothing));

            let global_method_index = self.runtime_methods.len();
            let impl_ptr = Pointer {
                addr: global_method_index,
                ty: ir::Type::Nothing,
            };
            
            let method_name_string = self.load_string_lit(method_info.name)?;

            let method_info_struct = StructValue::new(ir::METHODINFO_ID, [
                // 0: name
                method_name_string,
                // 1: owner
                DynValue::Pointer(typeinfo_ptr.clone()),
                // 2: impl
                DynValue::Pointer(impl_ptr),
                // 3: tags
                DynValue::Pointer(method_tags_array_ptr),
            ]);

            let method_info_ptr = self.new_object(method_info_struct, true)?;
            method_info_ptrs.push(DynValue::Pointer(method_info_ptr));

            self.runtime_methods.push(method_info.clone());
        }

        let method_array = self.new_dyn_array(&ir::METHODINFO_TYPE, method_info_ptrs, true)?;

        // update the typeinfo instance's methods field with the circular reference
        let methods_field_ptr = self.object_field_ptr(&typeinfo_ptr, ir::TYPEINFO_METHODS_FIELD)?;
        self.marshaller.marshal_at(&DynValue::from(method_array), &methods_field_ptr)?;

        Ok(DynValue::Pointer(typeinfo_ptr))
    }

    fn create_func_info_object(
        &mut self,
        func_id: ir::FunctionID,
        func_info: &ir::FunctionInfo,
    ) -> ExecResult<DynValue> {
        let func_name_string = self.load_string_lit(func_info.runtime_name
            .unwrap_or(ir::EMPTY_STRING_ID))?;

        let tags_loc = ir::TagLocation::Function(func_id);

        let ty_tags_array_ptr = self
            .get_tags_array_ptr(tags_loc)
            .unwrap_or_else(|| Pointer::nil(ir::Type::Nothing));

        let impl_ptr = Pointer {
            addr: func_id.0,
            ty: ir::Type::Nothing,
        };

        let fields = StructValue::new(ir::FUNCINFO_ID, [
            // 0: name
            func_name_string,
            // 1: impl
            DynValue::Pointer(impl_ptr),
            // 2: tags
            DynValue::Pointer(ty_tags_array_ptr),
        ]);

        let object_ptr = self.new_object(fields, true)?;
        Ok(DynValue::Pointer(object_ptr))
    }

    pub fn runtime_invoke(
        &mut self,
        func_id: ir::FunctionID,
        instance_arg_ref: Pointer,
        args_array_ptr: Pointer,
    ) -> ExecResult<(i32, Pointer)> {
        let Some(invoker_id) = self.functions
            .get(&func_id)
            .and_then(|f| f.invoker) 
        else {
            // not invokable
            const NOT_INVOKABLE_CODE: i32 = 2;
            return Ok((NOT_INVOKABLE_CODE, Pointer::nil(ir::Type::Nothing)));
        };

        // zeroed memory where a boxed result pointer may be stored
        let error_code_size = self.marshaller.get_ty(&ir::Type::I32)?.size();
        let mut error_code_mem: SmallVec<[u8; size_of::<i32>()]> = SmallVec::new();
        error_code_mem.resize(error_code_size, 0);

        let mut invoker_args = Vec::with_capacity(3);
        
        // self arg
        invoker_args.push(DynValue::Pointer(instance_arg_ref));

        // args array arg
        invoker_args.push(DynValue::Pointer(args_array_ptr));

        // error code out ptr arg
        invoker_args.push(DynValue::Pointer(Pointer::new(
            error_code_mem.as_mut_ptr() as usize,
            ir::Type::Nothing,
        )));

        // result is a box pointer
        let result_ptr = self
            .call(invoker_id, &invoker_args)?
            .and_then(|val| val.as_pointer().cloned())
            .ok_or_else(|| ExecError::illegal_state("expected invoker to return a pointer"))?;

        // error code should be set to 0 or a non-zero error value
        let result_code = self.marshaller
            .unmarshal(&error_code_mem, &ir::Type::I32)?
            .value
            .as_i32()
            .ok_or_else(|| ExecError::illegal_state("expected error code to contain a 32-bit int"))?;

        Ok((result_code, result_ptr))
    }

    fn update_diagnostics(&self) {
        if let Some(diag_worker) = &self.diag_worker {
            diag_worker.update(|| DiagnosticOutput {
                stack_trace: self.stack_trace(),
                heap_stats: self.native_heap.stats(),
            });
        }
    }

    pub fn shutdown(mut self) -> ExecResult<()> {
        let mut globals: Vec<_> = self.globals.values().cloned().collect();

        // first cleanup pass - release any global values which are left at shutdown time
        for i in (0..globals.len()).rev() {
            let global_val = &globals[i];
            let GlobalValue::Variable { value, ty } = global_val else {
                continue;
            };

            let ref_weak = match ty {
                ir::Type::Object(..) => Some(false),
                ir::Type::WeakObject(..) => Some(false),
                _ => None,
            };

            let dyn_val = self.marshaller.unmarshal(&value, &ty)?;

            if let Some(weak) = ref_weak {
                if self.release_dyn_val(&dyn_val.value, weak)? {
                    globals.remove(i);
                }
            }
        }

        if self.opts.leak_check {
            self.native_heap.check_leaks()?;
        }

        if self.opts.trace_heap {
            self.native_heap.print_trace();
        }

        if let Some(worker) = self.diag_worker.take() {
            worker.shutdown();
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
struct FunctionInfo {
    func: Rc<Function>,

    // cached debug name for stack frame
    name: Rc<String>,
    
    invoker: Option<ir::FunctionID>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct ExecOpts {
    pub trace_heap: bool,
    pub trace_rc: bool,
    pub trace_ir: bool,

    pub leak_check: bool,

    pub diag_port: u16,

    pub verbose: bool,
}

impl Default for ExecOpts {
    fn default() -> Self {
        Self {
            trace_heap: false,
            trace_rc: false,
            trace_ir: false,

            leak_check: false,

            diag_port: 0,
            verbose: false,
        }
    }
}

#[derive(Debug, Clone)]
enum GlobalValue {
    Variable { value: Box<[u8]>, ty: ir::Type },
    Function(ir::FunctionID),
    StaticTagArray(Pointer),
}

struct LabelLocation {
    pc_offset: usize,
}

fn find_labels(instructions: &[ir::Instruction]) -> HashMap<ir::Label, LabelLocation> {
    let mut locations = HashMap::new();

    for (pc_offset, instruction) in instructions.iter().enumerate() {
        match instruction {
            ir::Instruction::Label(label) => {
                locations.insert(label.clone(), LabelLocation {
                    pc_offset,
                });
            },
            _ => continue,
        }
    }

    locations
}
