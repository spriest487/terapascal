pub mod scope;

#[cfg(test)]
mod test;

use self::scope::*;
use crate::ast as ast;
use crate::codegen::library_builder::FunctionDeclKey;
use crate::codegen::library_builder::FunctionDefKey;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::metadata::*;
use crate::codegen::CodegenOpts;
use crate::codegen::FunctionInstance;
use crate::codegen::SetFlagsType;
use crate::ir::*;
use crate::typ as typ;
use crate::typ::seq::TypeSequenceSupport;
use crate::typ::Symbol;
use std::borrow::Cow;
use std::sync::Arc;
use terapascal_common::span::Span;
use terapascal_ir::instruction_builder::InstructionBuilder;

#[derive(Debug)]
pub struct Builder<'m, 'l: 'm> {
    library: &'m mut LibraryBuilder<'l>,

    // positional list of type args that can be used to reify types in the current context
    // during this stage we only need to be able to substitute args, we don't need to validate
    // anything, so combining them into a single list and ignoring positions is OK
    generic_context: typ::GenericContext,

    instructions: Vec<Instruction>,
    scopes: Vec<LocalScope>,
    next_label: Label,

    loop_stack: Vec<LoopScope>,
}

impl InstructionBuilder for Builder<'_, '_> {
    fn emit(&mut self, instruction: Instruction) {
        if instruction.should_discard() {
            return;
        }

        self.instructions.push(instruction);
    }

    fn metadata(&self) -> &Metadata {
        self.library.metadata()
    }

    fn is_debug(&self) -> bool {
        self.opts().debug
    }

    fn ir_formatter(&self) -> &impl IRFormatter {
        self.library.metadata()
    }

    fn local_temp(&mut self, ty: Type) -> LocalID {
        assert_ne!(Type::Nothing, ty);

        let id = self.current_scope_mut().bind_temp();
        self.instructions.push(Instruction::LocalAlloc(id, ty.clone()));

        id
    }

    fn next_label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label = Label(self.next_label.0 + 1);
        label
    }

    fn release(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        match ty {
            Type::RcPointer(..) => {
                self.emit(Instruction::Release { at: at.into(), weak: false });
                true
            }

            Type::RcWeakPointer(..) => {
                self.emit(Instruction::Release { at: at.into(), weak: true });
                true
            }

            _ => self.call_release(at.into(), ty),
        }
    }

    fn retain(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        match ty {
            Type::RcPointer(..) => {
                self.emit(Instruction::Retain { at: at.into(), weak: false });
                true
            }

            Type::RcWeakPointer(..) => {
                self.emit(Instruction::Retain { at: at.into(), weak: true });
                true
            }

            _ => self.call_retain(at.into(), ty),
        }
    }
}

impl<'m, 'l: 'm> Builder<'m, 'l> {
    pub fn new(lib: &'m mut LibraryBuilder<'l>) -> Self {
        let mut instructions = Vec::new();
        instructions.push(Instruction::LocalBegin);

        Self {
            library: lib,
            
            instructions,

            // the EXIT label is always reserved, so start one after that
            next_label: Label(EXIT_LABEL.0 + 1),
            scopes: vec![LocalScope::new(0)],

            loop_stack: Vec::new(),

            generic_context: typ::GenericContext::empty(),
        }
    }

    pub fn opts(&self) -> &CodegenOpts {
        &self.library.opts()
    }
    
    pub fn with_generic_ctx(mut self, ctx: typ::GenericContext) -> Self {
        self.generic_context = ctx;
        self
    }
    
    pub fn generic_context(&self) -> &typ::GenericContext {
        &self.generic_context
    }

    pub fn local_new(&mut self, ty: Type, name: Option<String>) -> LocalID {
        assert_ne!(Type::Nothing, ty);

        let id = self.current_scope_mut().bind_new(name.map(Arc::new), ty.clone());
        self.emit(Instruction::LocalAlloc(id, ty));

        id
    }

    pub fn translate_variant_case<'ty>(
        &'ty mut self,
        variant: &typ::Symbol,
        case: &str,
    ) -> (TypeDefID, usize, Option<&'ty Type>) {
        let name_path = self.translate_name(variant);

        let (id, variant_struct) = match self.library.metadata().find_variant_def(&name_path) {
            Some((id, variant_struct)) => (id, variant_struct),
            None => panic!("missing IR metadata definition for variant {}", variant),
        };

        let case_index = variant_struct
            .cases
            .iter()
            .position(|c| c.name.as_str() == case);

        match case_index {
            Some(index) => (id, index, variant_struct.cases[index].ty.as_ref()),
            None => panic!("missing case {} for {} in IR metadata", case, variant),
        }
    }

    pub fn translate_name(&mut self, name: &Symbol) -> NamePath {
        translate_name(name, &self.generic_context, self.library)
    }

    pub fn translate_iface(&mut self, iface_def: &typ::ast::InterfaceDecl) -> Interface {
        translate_iface(iface_def, &self.generic_context, self.library)
    }

    pub fn translate_type(&mut self, src_ty: &typ::Type) -> Type {
        self.library.translate_type(src_ty, &self.generic_context)
    }

    pub fn translate_dyn_array_struct(&mut self, element_ty: &typ::Type) -> TypeDefID {
        self.library.translate_dyn_array_struct(element_ty, &self.generic_context)
    }

    pub fn translate_method(
        &mut self,
        self_ty: typ::Type,
        self_ty_method_index: usize,
        mut call_ty_args: Option<typ::TypeArgList>
    ) -> FunctionInstance {
        if let Some(args_list) = &mut call_ty_args {
            *args_list = args_list
                .clone()
                .apply_type_args(&self.generic_context, &self.generic_context);
        }

        self.library.translate_method(self_ty, self_ty_method_index, call_ty_args)
    }

    pub fn translate_func(
        &mut self,
        decl_name: &Symbol,
        decl_sig: &Arc<typ::FunctionSig>,
        mut call_ty_args: Option<typ::TypeArgList>,
    ) -> FunctionInstance {
        if let Some(args_list) = &mut call_ty_args {
            *args_list = args_list
                .clone()
                .apply_type_args(&self.generic_context, &self.generic_context);
        }

        let mut key = FunctionDefKey {
            type_args: call_ty_args,
            decl_key: FunctionDeclKey::Function { 
                name: decl_name.full_path.clone(),
                sig: decl_sig.clone(),
            },
        };

        self.library.instantiate_func(&mut key)
    }

    pub fn translate_func_ty(&mut self, func_sig: &typ::FunctionSig) -> TypeDefID {
        self.library
            .translate_func_ty(func_sig, &self.generic_context)
    }

    pub fn build_closure_expr(&mut self, func: &typ::ast::AnonymousFunctionDef) -> Ref {
        let closure = self
            .library
            .build_closure_instance(func, &self.generic_context);

        if func.captures.len() == 0 {
            let static_closure = self.library.build_static_closure_instance(closure);

            Ref::Global(GlobalRef::StaticClosure(static_closure.id))
        } else {
            self.build_closure_instance(closure)
        }
    }
    
    pub fn build_function_closure(&mut self, func: &FunctionInstance) -> Ref {
        let static_closure = self
            .library
            .build_func_static_closure_instance(func, &self.generic_context);

        Ref::Global(GlobalRef::StaticClosure(static_closure.id))
    }

    pub fn build_closure_instance(&mut self, closure: ClosureInstance) -> Ref {
        let closure_def = self
            .library
            .metadata()
            .get_struct_def(closure.closure_id)
            .cloned()
            .unwrap();

        let closure_ptr_ty = closure.closure_ptr_ty();

        // virtual pointer to the closure
        let closure_ref = self.local_new(closure_ptr_ty.clone(), None);

        self.scope(|builder| {
            builder.rc_new(closure_ref.clone(), closure.closure_id, false);

            // the closure pointer type (a virtual pointer to any closure of this function type)
            // and the closure structure pointer type are different - we need to use the closure
            // *structure* pointer type to set members of this specific closure instance!
            let closure_struct_ty = Type::Struct(closure.closure_id);
            let closure_struct_ptr_ty = closure_struct_ty.clone().ptr();

            // downcast virtual closure ptr to the concrete closure struct
            let closure_struct_ref = Ref::from(builder.local_temp(closure_struct_ptr_ty.clone()));
            builder.cast(closure_struct_ref.clone(), closure_ref.clone(), closure_struct_ptr_ty.clone());

            let func_ptr_ty = closure_def.fields[&CLOSURE_PTR_FIELD].ty.clone();

            let func_field_ptr = builder.local_new(func_ptr_ty.clone().ptr(), None).to_ref();
            builder.field(
                func_field_ptr.clone(),
                closure_struct_ref.clone().to_deref(),
                closure_struct_ty.clone(),
                CLOSURE_PTR_FIELD,
            );

            // initialize closure reference to function
            let func_ref = Ref::Global(GlobalRef::Function(closure.func_instance.id));
            // builder.cast(func_field_ptr.clone().to_deref(), func_ref, func_ptr_ty.clone());
            builder.mov(func_field_ptr.clone().to_deref(), func_ref);
            
            // initialize closure capture fields - copy from local scope into closure object
            for (field_id, field_def) in closure_def.fields.iter() {
                // skip the closure pointer field
                if *field_id == CLOSURE_PTR_FIELD {
                    continue;
                }

                // skip unnamed (padding) fields
                let field_name = match &field_def.name {
                    None => continue,
                    Some(name) => name,
                };

                let capture_field_ptr = Ref::from(builder.local_temp(field_def.ty.clone().ptr()));
                builder.field(
                    capture_field_ptr.clone(),
                    closure_struct_ref.clone().to_deref(),
                    closure_struct_ty.clone(),
                    *field_id,
                );

                let capture_field = capture_field_ptr.to_deref();

                let captured_local_id = builder.find_local(field_name).unwrap().id();
                builder.mov(capture_field.clone(), captured_local_id);

                builder.retain(capture_field.clone(), &field_def.ty);
            }
        });

        closure_ref.to_ref()
    }
    
    pub fn get_method(&self, self_ty: &typ::Type, index: usize) -> typ::ast::MethodDecl {
        self.library.get_method(self_ty, index)
    }

    // for a given interface type and method index, get the method index from the self-type that
    // implements that method. panics on any error retrieving the methods, if the method index is
    // not valid for the interface type, or if the self-type is not an implementor of the interface
    pub fn get_impl_method_index(
        &self,
        self_ty: &typ::Type,
        iface_ty: &typ::Type,
        iface_method_index: usize,
    ) -> usize {
        if self_ty == iface_ty {
            return iface_method_index;
        }
        
        let iface_method = self.get_method(&iface_ty, iface_method_index);

        let method_name = iface_method.func_decl.ident();

        let impl_sig = iface_method.func_decl.sig().with_self(&self_ty);
        
        self.library.find_method_index(self_ty, method_name, &impl_sig)
    }

    pub fn pretty_ty_name(&self, ty: &Type) -> Cow<str> {
        self.library.metadata().pretty_ty_name(ty)
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringID {
        self.library.metadata_mut().find_or_insert_string(s)
    }

    pub fn get_struct(&self, id: TypeDefID) -> Option<&Struct> {
        self.library.metadata().get_struct_def(id)
    }

    #[allow(unused)]
    pub fn get_iface(&self, id: InterfaceID) -> Option<&Interface> {
        self.library.metadata().get_iface_def(id)
    }
    
    pub fn find_type_seq_support(&self, src_ty: &typ::Type) -> Option<TypeSequenceSupport> {
        self.library.find_type_seq_support(src_ty)
    }
    
    fn remove_empty_blocks(&mut self) {
        let mut pc = 0;

        // stack of instruction indices at which the current empty scope begins
        let mut empty = vec![Some(pc)];

        while pc < self.instructions.len() {
            let block_empty = empty.last_mut().unwrap();

            match &self.instructions[pc] {
                Instruction::LocalBegin => {
                    empty.push(Some(pc));
                    pc += 1;
                }

                // end the scope
                Instruction::LocalEnd => {
                    if let Some(empty_start) = *block_empty {
                        // it's still empty, remove all the empty statements
                        let remove_count = (pc + 1) - empty_start;
                        pc = empty_start;
                        for _ in 0..remove_count {
                            self.instructions.remove(pc);
                        }

                        empty.pop();
                    } else {
                        empty.pop();
                        
                        // containing scope is no longer empty
                        *empty.last_mut().unwrap() = None;

                        pc += 1;
                    }
                }
                
                Instruction::DebugPop
                | Instruction::DebugPush(..)
                | Instruction::Comment(..) => {
                    pc += 1;
                }
                
                _ => {
                    *block_empty = None;
                    pc += 1;
                }
            }
        }
    }

    pub fn finish(mut self) -> Vec<Instruction> {
        while !self.scopes.is_empty() {
            self.end_scope();
        }

        self.remove_empty_blocks();

        if matches!(self.instructions.as_slice(), [
            Instruction::DebugPush(..),
            Instruction::DebugPop,
        ]) {
            self.instructions.pop();
        }

        self.instructions
    }

    pub fn push_debug_context(&mut self, ctx: Span) {
        if !self.opts().debug {
            return;
        }

        self.current_scope_mut().inc_debug_ctx_count();

        self.emit(Instruction::DebugPush(ctx));
    }

    pub fn pop_debug_context(&mut self) {
        if !self.opts().debug {
            return;
        }

        self.current_scope_mut().dec_debug_ctx_count();

        self.emit(Instruction::DebugPop);
    }
    
    pub fn set_include(&mut self, set_ref: impl Into<Ref>, bit_val: impl Into<Value>, set_type: &typ::SetType) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);
        
        let flags_ptr = self.local_temp(flags_type.ptr());
        self.addr_of(flags_ptr.clone(), set_ref);

        self.call(flags_type_info.include_func, [
            Value::from(flags_ptr),
            bit_val.into(),
        ], None);
    }

    // todo: what is this for
    #[allow(unused)]
    pub fn set_exclude(&mut self, set_ref: impl Into<Ref>, bit_val: impl Into<Value>, set_type: &typ::SetType) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);

        let flags_ptr = self.local_temp(flags_type.ptr());
        self.addr_of(flags_ptr.clone(), set_ref);

        self.call(flags_type_info.exclude_func, [
            Value::from(flags_ptr),
            bit_val.into(),
        ], None);
    }

    pub fn set_contains(&mut self,
        out: impl Into<Ref>,
        set_ref: impl Into<Ref>,
        bit_val: impl Into<Value>,
        set_type: &typ::SetType
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);

        let flags_ptr = self.local_temp(flags_type.ptr());
        self.addr_of(flags_ptr.clone(), set_ref);

        self.call(flags_type_info.contains_func, [
            Value::from(flags_ptr),
            bit_val.into(),
        ], Some(out.into()));
    }
    
    pub fn set_eq(&mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);

        let a_ptr = self.local_temp(flags_type.clone().ptr());
        let b_ptr = self.local_temp(flags_type.ptr());
        self.addr_of(a_ptr.clone(), a);
        self.addr_of(b_ptr.clone(), b);

        self.call(flags_type_info.eq_func, [
            Value::from(a_ptr),
            Value::from(b_ptr),
        ], Some(out.into()));
    }

    pub fn set_bit_not(&mut self,
        a: impl Into<Ref>,
        set_type: &typ::SetType
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);

        let a_ptr = self.local_temp(flags_type.clone().ptr());
        self.addr_of(a_ptr.clone(), a);

        self.call(flags_type_info.bit_not_func, [
            Value::from(a_ptr),
        ], None);
    }

    fn set_bitwise_op(&mut self,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType,
        get_func_id: fn(&SetFlagsType) -> FunctionID,
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Struct(flags_type_info.struct_id);

        let a_ptr = self.local_temp(flags_type.clone().ptr());
        let b_ptr = self.local_temp(flags_type.ptr());
        self.addr_of(a_ptr.clone(), a);
        self.addr_of(b_ptr.clone(), b);

        self.call(get_func_id(&flags_type_info), [
            Value::from(a_ptr),
            Value::from(b_ptr),
        ], None);
    }

    pub fn set_bit_and(&mut self,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType,
    ) {
        self.set_bitwise_op(a, b, set_type, |i| i.bit_and_func)
    }

    pub fn set_bit_or(&mut self,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType,
    ) {
        self.set_bitwise_op(a, b, set_type, |i| i.bit_or_func)
    }

    pub fn set_bit_xor(&mut self,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType,
    ) {
        self.set_bitwise_op(a, b, set_type, |i| i.bit_xor_func)
    }

    pub fn get_mem(&mut self, count: impl Into<Value>, out: impl Into<Ref>) {
        let function_ref = Ref::Global(GlobalRef::Function(self.library.instantiate_get_mem_func()));
        self.call(function_ref, [count.into()], Some(out.into()));
    }

    pub fn free_mem(&mut self, at: impl Into<Value>) {
        let function_ref = Ref::Global(GlobalRef::Function(self.library.instantiate_free_mem_func()));
        self.call(function_ref, [at.into()], None);
    }

    pub fn bind_param(&mut self, ty: Type, name: impl Into<String>, by_ref: bool) -> LocalID {
        self.current_scope_mut().bind_param(Some(Arc::new(name.into())), ty, by_ref)
    }

    // binds an anonymous return local in %0 with the indicated type
    pub fn bind_return(&mut self) {
        self.current_scope_mut().bind_return();
    }

    // binds an anonymous local binding for the closure pointer of a function
    pub fn bind_closure_ptr(&mut self) -> LocalID {
        self.current_scope_mut().bind_temp()
    }

    fn current_scope(&mut self) -> &LocalScope {
        self.scopes
            .iter()
            .rev()
            .next()
            .expect("scope must be active")
    }

    fn current_scope_mut(&mut self) -> &mut LocalScope {
        self.scopes
            .iter_mut()
            .rev()
            .next()
            .expect("scope must be active")
    }

    pub fn find_local(&self, name: &str) -> Option<&Local> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.local_by_name(name)
                .and_then(|id| scope.local_by_id(id)))
    }
    
    pub fn find_global_var(&self, name_path: &ast::IdentPath) -> Option<VariableID> {
        self.library.find_global_var(name_path)
    }

    pub fn local_closure_capture(&mut self, ty: Type, name: String) -> Ref {
        assert_ne!(Type::Nothing, ty);

        let id = self.current_scope_mut().bind_param(Some(Arc::new(name)), ty.clone(), true);
        self.emit(Instruction::LocalAlloc(id, ty.clone()));

        Ref::Local(id)
    }

    fn call_release(&mut self, at: Ref, ty: &Type) -> bool {
        let rc_funcs = self.library.gen_runtime_type(ty);
        let Some(release) = rc_funcs.release else {
            return false; 
        };

        let at_ptr = self.local_temp(ty.clone().ptr());
        self.addr_of(at_ptr.clone(), at);
        self.call(release, [Value::from(at_ptr)], None);

        true
    }
    
    fn call_retain(&mut self, at: Ref, ty: &Type) -> bool {
        let rc_funcs = self.library.gen_runtime_type(ty);

        let Some(retain) = rc_funcs.retain else {
            return false;
        };

        let at_ptr = self.local_temp(ty.clone().ptr());
        self.addr_of(at_ptr.clone(), at);
        self.call(retain, [Value::Ref(Ref::from(at_ptr))], None);
        
        true
    }

    pub fn begin_loop_body_scope(&mut self, continue_label: Label, break_label: Label) {
        self.loop_stack.push(LoopScope {
            continue_label,
            break_label,
            block_level: self.scopes.len(),
        });

        self.begin_scope();
    }

    pub fn end_loop_body_scope(&mut self) {
        self.end_scope();

        self.loop_stack
            .pop()
            .expect("end_loop called without an active loop");
    }

    pub fn loop_body_scope<F>(
        &mut self,
        continue_label: Label,
        break_label: Label,
        f: F,
    ) -> &[Instruction]
    where
        F: FnOnce(&mut Self),
    {
        let start_instruction = self.instructions.len();

        self.begin_loop_body_scope(continue_label, break_label);

        f(self);

        self.end_loop_body_scope();

        &self.instructions[start_instruction..]
    }

    pub fn current_loop(&self) -> Option<&LoopScope> {
        self.loop_stack.last()
    }

    pub fn begin_scope(&mut self) {
        self.emit(Instruction::LocalBegin);

        let scope = self.current_scope().new_child();
        self.scopes.push(scope);

        self.comment(&format!("begin scope {}", self.scopes.len()));
    }

    pub fn scope<F>(&mut self, f: F) -> &[Instruction]
    where
        F: FnOnce(&mut Self),
    {
        let start_index = self.instructions.len();

        self.begin_scope();
        f(self);
        self.end_scope();

        &self.instructions[start_index..]
    }

    /// release locals in all scopes after the position indicated by
    /// `to_scope` in the scope stack
    /// this should be used when jumping out a scope, or before popping one
    fn cleanup_scope(&mut self, to_scope: usize) {
        assert!(
            self.scopes.len() > to_scope,
            "reset_scope index out of range: {}",
            to_scope
        );

        let last_scope = self.scopes.len() - 1;

        if self.opts().debug {
            if to_scope == last_scope {
                self.comment(&format!("cleanup scope {}", to_scope + 1));
            } else {
                self.comment(&format!(
                    "cleanup scopes {}..{}",
                    to_scope + 1,
                    self.scopes.len()
                ));
            }
        }

        let cleanup_range = to_scope..=last_scope;

        if self.opts().debug {
            let debug_pops: usize = self.scopes[cleanup_range.clone()]
                .iter()
                .map(|scope| scope.debug_ctx_count())
                .sum();

            for _ in 0..debug_pops {
                // don't call the helper func to do this, we don't want to modify the scope here
                self.emit(Instruction::DebugPop);
            }
        }

        // locals from all scopes up to the target scope, in order of deepest->shallowest,
        // then in reverse allocation order
        let locals: Vec<_> = self.scopes[cleanup_range]
            .iter()
            .rev()
            .flat_map(|scope| scope.locals().iter().rev().cloned())
            .collect();

        // release local bindings that will be lost when the current scope is popped.
        // of course. releasing a ref should either insert a release instruction directly
        // (for an RC pointer) or insert a call to a structural release function (for
        // complex types containing RC pointers), so should never introduce new locals
        // in the scope being popped
        for local in locals {
            if self.opts().annotate_rc {
                self.comment(&format!("expire {}", local.id()));
            }

            match local {
                Local::Param { id, ty, by_ref, .. } => {
                    if !by_ref {
                        self.release(id, &ty);
                    }
                },

                Local::New { id, ty, .. } => {
                    self.release(Ref::Local(id), &ty);
                },

                Local::Temp { .. } => {
                    // no cleanup required
                },

                Local::Return { .. } => {
                    if self.opts().annotate_rc {
                        self.comment("expire return slot");
                    }
                },
            }
        }
    }

    pub fn end_scope(&mut self) {
        self.cleanup_scope(self.scopes.len() - 1);

        self.comment(&format!("end scope {}", self.scopes.len()));

        if self.scopes.pop().is_none() {
            panic!("mismatched begin/end scope calls, no scope to pop");
        }

        self.emit(Instruction::LocalEnd);
    }

    pub fn break_loop(&mut self) {
        let (break_label, break_scope) = {
            let current_loop = self
                .current_loop()
                .expect("break stmt must appear in a loop");

            (current_loop.break_label, current_loop.block_level)
        };

        // write cleanup code for the broken scope and its children
        self.cleanup_scope(break_scope);

        // jump to the label (presumably somewhere outside the broken scope!)
        self.emit(Instruction::Jump { dest: break_label });
    }

    pub fn continue_loop(&mut self) {
        let (continue_label, continue_scope) = {
            let current_loop = self
                .current_loop()
                .expect("continue stmt must appear in a loop");

            (current_loop.continue_label, current_loop.block_level)
        };

        self.cleanup_scope(continue_scope);
        self.emit(Instruction::Jump {
            dest: continue_label,
        });
    }
    
    pub fn bounds_check(&mut self,
        element_ty: &Type,
        length: impl Into<Value>,
        index: impl Into<Value>
    ) {
        let bounds_check_func = self.library.gen_bounds_check(element_ty);
        let func_ref = Value::Ref(Ref::Global(GlobalRef::Function(bounds_check_func)));

        self.call(func_ref, [
            length.into(),
            index.into(),
        ], None);
    }

    pub fn exit_function(&mut self) {
        self.cleanup_scope(0);

        self.emit(Instruction::Jump { dest: EXIT_LABEL })
    }
}

pub fn jmp_exists(instructions: &[Instruction], to_label: Label) -> bool {
    instructions.iter().any(|i| match i {
        Instruction::Jump { dest } | Instruction::JumpIf { dest, .. } => *dest == to_label,
        _ => false,
    })
}
