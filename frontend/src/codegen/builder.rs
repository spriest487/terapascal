#[cfg(test)]
mod test;

use crate::ast as ast;
use crate::codegen::expr::literal_to_val;
use crate::codegen::library_builder::FunctionDeclKey;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::library_builder::RcMethodInfo;
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
use terapascal_ir::InstructionBuilder;
use terapascal_ir::LocalStack;

#[derive(Debug)]
pub struct IRBuilder<'m, 'l: 'm> {
    library: &'m mut LibraryBuilder<'l>,

    instructions: Vec<DebugInstruction>,
    next_label: Label,

    local_stack: LocalStack,

    debug_stack: Vec<Span>,
}

#[derive(Debug, Clone)]
pub struct DebugInstruction {
    instruction: Instruction,
    debug_span: Option<Span>,
}

impl AsInstruction for DebugInstruction {
    fn as_instruction(&self) -> &Instruction {
        &self.instruction
    }
}

impl InstructionBuilder for IRBuilder<'_, '_> {
    fn emit(&mut self, instruction: Instruction) {
        if instruction.should_discard() {
            return;
        }

        self.instructions.push(DebugInstruction {
            instruction,
            debug_span: self.debug_stack.last().cloned(),
        });
    }

    fn metadata(&self) -> &impl MetadataSource {
        self.library.metadata()
    }

    fn local_stack(&self) -> &LocalStack {
        &self.local_stack
    }

    fn local_stack_mut(&mut self) -> &mut LocalStack {
        &mut self.local_stack
    }

    fn is_debug(&self) -> bool {
        self.opts().debug
    }

    fn ir_formatter(&self) -> &impl IRFormatter {
        self.library.metadata()
    }

    fn next_label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label = Label(self.next_label.0 + 1);
        label
    }

    fn push_source(&mut self, ctx: Span) {
        if !self.opts().debug {
            return;
        }

        self.debug_stack.push(ctx);
    }

    fn pop_source(&mut self) {
        if !self.opts().debug {
            return;
        }

        if self.debug_stack.pop().is_none() {
            eprintln!("pop_debug_context: debug context stack is empty");
        }
    }

    fn release_deep(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        if ty.is_object() {
            self.comment(format!("release: {}", self.metadata().pretty_type_name(ty)));
            self.release(at, ty.is_weak(), Ref::Discard);
            return true;
        }

        if let Type::Generic(param_name) = ty {
            // placeholder release to be expanded during instantiation
            self.comment(format!("release (generic): {}", param_name));
            return true;
        }

        let rc_method_info = self.get_rc_method_info(ty);
        let Some(func_id) = rc_method_info.release_elements else {
            return false;
        };

        if self.opts().debug {
            self.comment(format!("release_deep: {}", self.metadata().pretty_type_name(ty)));
        }

        let at_ref = self.make_ref_local(at, ty);
        self.call(func_id, [at_ref.value()], [], None);
        true
    }

    fn retain_deep(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        if ty.is_object() {
            self.comment(format!("retain: {}", self.metadata().pretty_type_name(ty)));
            self.retain(at, ty.is_weak());
            return true;
        }

        if let Type::Generic(param_name) = ty {
            // placeholder release to be expanded during instantiation
            self.comment(format!("retain (generic): {}", param_name));
            return true;
        }

        let rc_method_info = self.get_rc_method_info(ty);
        let Some(func_id) = rc_method_info.retain_elements else {
            return false;
        };

        if self.opts().debug {
            self.comment(format!("retain_deep: {}", self.metadata().pretty_type_name(ty)));
        }

        let at_ref = self.make_ref_local(at, ty);
        self.call(func_id, [at_ref.value()], [], None);
        true
    }
}

impl<'m, 'l: 'm> IRBuilder<'m, 'l> {
    pub fn new(lib: &'m mut LibraryBuilder<'l>) -> Self {
        Self {
            library: lib,
            
            instructions: Vec::new(),

            // the EXIT label is always reserved, so start one after that
            next_label: Label(EXIT_LABEL.0 + 1),
            local_stack: LocalStack::new(),

            debug_stack: Vec::new(),
        }
    }

    pub fn opts(&self) -> &CodegenOpts {
        &self.library.opts()
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
    
    pub fn literal_to_val(&mut self, lit: &typ::ast::Literal, ty: &typ::Type) -> Value {
        let ty = self.translate_type(ty);
        literal_to_val(lit, &ty, self.library)
    }
    
    pub fn translate_literal(&mut self, lit: &typ::ast::Literal, ty: &typ::Type) -> Ref {
        let val = self.literal_to_val(lit, ty);

        if let Value::Ref(lit_ref) = val {
            return lit_ref
        };
        
        let out_ty = self.translate_type(ty);
        let out = self.local_temp(out_ty);

        self.mov(out, val);

        out.to_ref()
    }

    pub fn translate_name(&mut self, name: &Symbol) -> NamePath {
        translate_name(name, self.library)
    }

    pub fn translate_iface(&mut self, iface_def: &typ::ast::InterfaceDecl) -> InterfaceDef {
        translate_iface(iface_def, self.library)
    }

    pub fn translate_type(&mut self, src_ty: &typ::Type) -> Type {
        self.library.translate_type(src_ty)
    }

    pub fn translate_method(
        &mut self,
        self_ty: typ::Type,
        self_ty_method_index: usize,
    ) -> FunctionInstance {
        self.library.translate_method(self_ty, self_ty_method_index)
    }

    pub fn translate_func(
        &mut self,
        decl_name: &Symbol,
        decl_sig: &Arc<typ::FunctionSig>,
    ) -> FunctionInstance {
        let key = FunctionDeclKey::Function {
            name: decl_name.full_path.clone(),
            sig: decl_sig.clone(),
        };

        self.library.instantiate_func(&key)
    }

    pub fn translate_func_ty(&mut self, func_sig: &typ::FunctionSig) -> TypeDefID {
        self.library.translate_func_ty(func_sig)
    }

    pub fn build_closure_expr(&mut self, func: &typ::ast::AnonymousFunctionDef) -> Ref {
        let closure = self
            .library
            .build_closure_instance(func);

        if func.captures.len() == 0 {
            let static_closure = self.library.build_static_closure_instance(closure);
            let static_closure_ref = Ref::Global(GlobalRef::Variable(static_closure.id));

            // closure objects have a specific type, but refs to closures are type erased so
            // we need to cast to Object. we know static closures are immortal, so this can be
            // a temp ref
            let closure_obj = self.local_temp(ANY_TYPE);
            self.cast(closure_obj, static_closure_ref, ANY_TYPE);
            
            closure_obj.to_ref()
        } else {
            self.build_closure_instance(closure, false)
        }
    }
    
    pub fn build_function_closure(&mut self, func: &FunctionInstance) -> Ref {
        let static_closure = self
            .library
            .build_func_static_closure_instance(func);

        Ref::Global(GlobalRef::Variable(static_closure.id))
    }

    pub fn build_closure_instance(&mut self, closure: ClosureInstance, immortal: bool) -> Ref {
        let closure_def = self
            .library
            .metadata()
            .get_struct_def(closure.closure_id)
            .cloned()
            .unwrap();

        // this is a *specific* closure class, not a virtual (function typed) pointer
        let closure_ptr_ty = closure.closure_id.to_class_ptr_type();

        // virtual pointer to the closure
        let closure_virtual_ty = Type::Object(ObjectID::Closure(closure.func_ty_id));
        let closure_virtual_ptr = self.local_var(closure_virtual_ty.clone(), None);

        self.scope(|builder| {
            let closure_ref = builder.local_temp(closure_ptr_ty.clone());
            builder.new_object(closure_ref, closure.closure_id, immortal);

            let func_field_ref = closure_ref.to_ref().field_ref(closure_ptr_ty.clone(), CLOSURE_PTR_FIELD);

            // initialize closure reference to function
            let func_ref = Ref::Global(GlobalRef::Function(closure.func_instance.id));
            // builder.cast(func_field_ptr.clone().to_deref(), func_ref, func_ptr_ty.clone());
            builder.mov(func_field_ref.to_deref(), func_ref);
            
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

                let capture_field_ref = closure_ref.to_ref().field_ref(closure_ptr_ty.clone(), *field_id);

                let captured_local = builder.find_named(field_name).unwrap();
                builder.mov(capture_field_ref.to_deref(), captured_local.to_ref());
                builder.retain_deep(capture_field_ref.to_deref(), &field_def.ty);
            }
            
            builder.cast(closure_virtual_ptr, closure_ref, closure_virtual_ty);
        });

        self.retain(closure_virtual_ptr, false);
        closure_virtual_ptr.to_ref()
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

    pub fn pretty_ty_name(&self, ty: &Type) -> Cow<'_, str> {
        self.library.metadata().pretty_type_name(ty)
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> StringID {
        self.library.metadata_mut().find_or_insert_string(s)
    }

    pub fn get_struct(&self, id: TypeDefID) -> Option<&StructDef> {
        self.library.metadata().get_struct_def(id)
    }

    #[allow(unused)]
    pub fn get_iface(&self, id: InterfaceID) -> Option<&InterfaceDef> {
        self.library.metadata().get_iface_def(id)
    }
    
    pub fn find_type_seq_support(&self, src_ty: &typ::Type) -> Option<TypeSequenceSupport> {
        self.library.find_type_seq_support(src_ty)
    }

    pub fn finish(mut self) -> InstructionList {
        while self.local_stack.len() > 0 {
            self.local_end();
        }

        let var_count = self.local_stack.local_slot_count();
        let instruction_count = self.instructions.len() + var_count;

        let mut instructions = Vec::with_capacity(instruction_count);
        let mut sources = Vec::with_capacity(instruction_count);

        for (local_id, ty) in self.local_stack.finish() {
            instructions.push(Instruction::LocalAlloc(local_id, ty));
            sources.push(None);
        }

        for instruction in self.instructions {
            instructions.push(instruction.instruction);
            sources.push(instruction.debug_span);
        }

        InstructionList {
            instructions,
            sources,
        }
    }
    
    pub fn set_include(&mut self, set_ref: impl Into<Ref>, bit_val: impl Into<Value>, set_type: &typ::SetType) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Flags(flags_type_info.struct_id);
        
        let flags_ptr = self.local_temp(flags_type.temp_ref());
        self.make_ref(flags_ptr, set_ref);

        self.call(flags_type_info.include_func, [
            flags_ptr.value(),
            bit_val.into(),
        ], [], None);
    }

    // todo: what is this for
    #[allow(unused)]
    pub fn set_exclude(&mut self, set_ref: impl Into<Ref>, bit_val: impl Into<Value>, set_type: &typ::SetType) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Flags(flags_type_info.struct_id);

        let flags_ptr = self.local_temp(flags_type.temp_ref());
        self.make_ref(flags_ptr, set_ref);

        self.call(flags_type_info.exclude_func, [
            flags_ptr.value(),
            bit_val.into(),
        ], [], None);
    }

    pub fn set_contains(&mut self,
        out: impl Into<Ref>,
        set_ref: impl Into<Ref>,
        bit_val: impl Into<Value>,
        set_type: &typ::SetType
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Flags(flags_type_info.struct_id);

        let flags_ptr = self.local_temp(flags_type.temp_ref());
        self.make_ref(flags_ptr, set_ref);

        self.call(flags_type_info.contains_func, [
            flags_ptr.value(),
            bit_val.into(),
        ], [], Some(out.into()));
    }
    
    pub fn set_eq(&mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Flags(flags_type_info.struct_id);

        let a_ptr = self.local_temp(flags_type.temp_ref());
        let b_ptr = self.local_temp(flags_type.temp_ref());
        self.make_ref(a_ptr, a);
        self.make_ref(b_ptr, b);

        self.call(flags_type_info.eq_func, [
            a_ptr.value(),
            b_ptr.value(),
        ], [], Some(out.into()));
    }

    pub fn set_bit_not(&mut self,
        a: impl Into<Ref>,
        set_type: &typ::SetType
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Flags(flags_type_info.struct_id);

        let a_ptr = self.local_temp(flags_type.temp_ref());
        self.make_ref(a_ptr, a);

        self.call(flags_type_info.bit_not_func, [
            a_ptr.value(),
        ], [], None);
    }

    fn set_bitwise_op(&mut self,
        a: impl Into<Ref>,
        b: impl Into<Ref>,
        set_type: &typ::SetType,
        get_func_id: fn(&SetFlagsType) -> FunctionID,
    ) {
        let flags_type_info = self.library.get_set_flags_type_info(set_type.flags_type_bits());
        let flags_type = Type::Flags(flags_type_info.struct_id);

        let a_ptr = self.local_temp(flags_type.temp_ref());
        let b_ptr = self.local_temp(flags_type.temp_ref());
        self.make_ref(a_ptr, a);
        self.make_ref(b_ptr, b);

        self.call(get_func_id(&flags_type_info), [
            a_ptr.value(),
            b_ptr.value(),
        ], [], None);
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

    #[expect(unused)]
    pub fn get_mem(&mut self, count: impl Into<Value>, out: impl Into<Ref>) {
        let function_ref = Ref::Global(GlobalRef::Function(self.library.instantiate_get_mem_func()));
        self.call(function_ref, [count.into()], [], Some(out.into()));
    }

    #[expect(unused)]
    pub fn get_mem_id(&mut self) -> FunctionID {
        self.library.instantiate_get_mem_func()
    }

    #[expect(unused)]
    pub fn free_mem(&mut self, at: impl Into<Value>) {
        let function_ref = Ref::Global(GlobalRef::Function(self.library.instantiate_free_mem_func()));
        self.call(function_ref, [at.into()], [], None);
    }

    pub fn bind_param(&mut self, id: ArgID, ty: Type, name: impl Into<String>) {
        self.local_stack_mut().bind_param(id, ty, Arc::new(name.into()), false)
    }

    pub fn bind_ref_param(&mut self, id: ArgID, ty: Type, name: impl Into<String>) {
        self.local_stack_mut().bind_param(id, ty.temp_ref(), Arc::new(name.into()), true)
    }

    // binds an anonymous return local in %0 with the indicated type
    pub fn bind_return(&mut self, ty: Type) {
        self.local_stack_mut().bind_return(ty);
    }

    // binds an anonymous local binding for the closure pointer of a function
    pub fn bind_closure_ptr(&mut self, id: ArgID) {
        self.local_stack_mut().bind_unnamed_param(id, ANY_TYPE, false)
    }

    pub fn find_global_var(&self, name_path: &ast::IdentPath) -> Option<VariableID> {
        self.library.find_global_var(name_path)
    }

    pub fn local_closure_capture(&mut self, ty: Type, name: String) -> Ref {
        assert_ne!(Type::Nothing, ty);

        let id = self.local_stack_mut().bind_capture(ty, name);
        
        Ref::Local(id)
    }

    pub fn begin_loop_body_scope(&mut self, continue_label: Label, break_label: Label) {
        self.local_stack_mut().push_loop(continue_label, break_label);
        self.local_begin();
    }

    pub fn end_loop_body_scope(&mut self) {
        self.local_end();
        self.local_stack_mut().pop_loop();
    }

    pub fn loop_body_scope<F>(
        &mut self,
        continue_label: Label,
        break_label: Label,
        f: F,
    ) -> &[DebugInstruction]
    where
        F: FnOnce(&mut Self),
    {
        let start_instruction = self.instructions.len();

        self.begin_loop_body_scope(continue_label, break_label);

        f(self);

        self.end_loop_body_scope();

        &self.instructions[start_instruction..]
    }

    pub fn scope<F>(&mut self, f: F) -> &[DebugInstruction]
    where
        F: FnOnce(&mut Self),
    {
        let start_index = self.instructions.len();

        self.local_begin();
        f(self);
        self.local_end();

        &self.instructions[start_index..]
    }

    pub fn array_bounds_check(&mut self,
        length: impl Into<Value>,
        index: impl Into<Value>
    ) {
        self.comment("array bounds check");

        let index_val = index.into();
        let length_val = length.into();
        let bounds_ok_label = self.next_label();

        // if index >= 0 and index < arr.len then goto "bounds_ok"
        let gte_zero = self.gte_to_val(index_val.clone(), Value::LiteralI32(0));
        let lt_len = self.lt_to_val(index_val, length_val);
        let bounds_check_ok = self.and_to_val(gte_zero, lt_len);

        self.jmpif(bounds_ok_label, bounds_check_ok);

        // otherwise: raise
        let err_str = self.find_or_insert_string("array index out of bounds");
        self.emit(Instruction::Raise {
            val: Ref::Global(GlobalRef::StringLiteral(err_str)),
        });

        self.label(bounds_ok_label);
    }

    pub fn exit_function(&mut self) {
        self.cleanup_scope(0);

        self.emit(Instruction::Jump { dest: EXIT_LABEL })
    }
    
    pub fn get_rc_method_info(&mut self, ty: &Type) -> RcMethodInfo {
        self.library.get_rc_method_info(ty)
    }
}
