#[cfg(test)]
mod test;

use crate::codegen::expr::literal_to_val;
use crate::codegen::library_builder::FunctionDeclKey;
use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::library_builder::MethodDeclKey;
use crate::codegen::metadata::*;
use crate::codegen::CodegenOpts;
use crate::codegen::FunctionInstance;
use crate::codegen::SetFlagsType;
use crate::ir as ir;
use crate::typ as typ;
use crate::typ::seq::TypeSequenceSupport;
use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Arc;
use terapascal_common::ident::IdentPath;
use terapascal_common::span::Span;
use ir::InstructionBuilder;
use ir::MetadataSource;

#[derive(Debug)]
pub struct IRBuilder<'m, 'l: 'm> {
    library: &'m mut LibraryBuilder<'l>,

    instructions: Vec<DebugInstruction>,
    next_label: ir::Label,

    local_stack: ir::LocalStack,

    debug_stack: Vec<Span>,
}

#[derive(Debug, Clone)]
pub struct DebugInstruction {
    instruction: ir::Instruction,
    debug_span: Option<Span>,
}

impl ir::AsInstruction for DebugInstruction {
    fn as_instruction(&self) -> &ir::Instruction {
        &self.instruction
    }
}

impl InstructionBuilder for IRBuilder<'_, '_> {
    fn emit(&mut self, instruction: ir::Instruction) {
        if instruction.should_discard() {
            return;
        }

        self.instructions.push(DebugInstruction {
            instruction,
            debug_span: self.debug_stack.last().cloned(),
        });
    }

    fn metadata(&self) -> &impl ir::MetadataSource {
        self.library.metadata()
    }

    fn local_stack(&self) -> &ir::LocalStack {
        &self.local_stack
    }

    fn local_stack_mut(&mut self) -> &mut ir::LocalStack {
        &mut self.local_stack
    }

    fn is_debug(&self) -> bool {
        self.opts().debug
    }

    fn ir_formatter(&self) -> &impl ir::IRFormatter {
        self.library.metadata()
    }

    fn next_label(&mut self) -> ir::Label {
        let label = self.next_label;
        self.next_label = ir::Label(self.next_label.0 + 1);
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
}

impl<'m, 'l: 'm> IRBuilder<'m, 'l> {
    pub fn new(lib: &'m mut LibraryBuilder<'l>) -> Self {
        Self {
            library: lib,
            
            instructions: Vec::new(),

            // the EXIT label is always reserved, so start one after that
            next_label: ir::Label(ir::EXIT_LABEL.0 + 1),
            local_stack: ir::LocalStack::new(),

            debug_stack: Vec::new(),
        }
    }

    pub fn opts(&self) -> &CodegenOpts {
        &self.library.opts()
    }

    pub fn translate_variant_case(
        &mut self,
        variant: &typ::Symbol,
        case: &str,
    ) -> (Rc<ir::TypeRef>, usize, Option<ir::Type>) {
        let variant_src_type = typ::Type::variant(variant.clone());

        let ir::Type::Variant(id) = self.translate_type(&variant_src_type) else {
            unreachable!("result of translating variant must be variant");
        };

        let variant_def = self.library
            .metadata()
            .instantiate_variant_def(id.def_id, &id.args)
            .unwrap_or_else(|| {
                panic!("missing IR metadata definition for variant {}", variant)
            });

        let Some(case_index) = variant_def
            .cases
            .iter()
            .position(|c| c.name.as_str() == case)
        else {
            panic!("missing case {} for {} in IR metadata", case, variant)
        };

        (id, case_index, variant_def.cases[case_index].ty.clone())
    }

    pub fn translate_set_type(&mut self, set_type: &Arc<typ::SetDef>) -> SetFlagsType {
        self.library.translate_set_type(set_type)
    }
    
    pub fn literal_to_val(&mut self, lit: &typ::ast::Literal, ty: &typ::Type) -> ir::Value {
        let ty = self.translate_type(ty);
        literal_to_val(lit, &ty, self.library)
    }
    
    pub fn translate_literal(&mut self, lit: &typ::ast::Literal, ty: &typ::Type) -> ir::Ref {
        let val = self.literal_to_val(lit, ty);

        if let ir::Value::Ref(lit_ref) = val {
            return lit_ref
        };
        
        let out_ty = self.translate_type(ty);
        let out = self.local_temp(out_ty);

        self.mov(out, val);

        out.to_ref()
    }

    #[expect(unused)]
    pub fn translate_name(&mut self, name: &typ::Symbol) -> ir::NamePath {
        translate_name(name, self.library)
    }

    #[expect(unused)]
    pub fn translate_iface(&mut self, id: ir::InterfaceID, iface_def: &typ::ast::InterfaceDecl) -> ir::InterfaceDef {
        translate_iface(id, iface_def, self.library)
    }

    pub fn translate_type(&mut self, src_ty: &typ::Type) -> ir::Type {
        self.library.translate_type(src_ty)
    }

    pub fn translate_sig(&mut self, sig: &typ::FunctionSig) -> ir::FunctionSig {
        translate_sig(sig, self.library)
    }

    pub fn translate_method(
        &mut self,
        self_ty: typ::Type,
        self_ty_method_index: usize,
    ) -> FunctionInstance {
        let key = FunctionDeclKey::Method(MethodDeclKey {
            self_ty,
            method_index: self_ty_method_index,
        });

        self.library.instantiate_func(&key)
    }

    pub fn translate_func(
        &mut self,
        decl_name: &typ::Symbol,
        decl_sig: &Arc<typ::FunctionSig>,
    ) -> FunctionInstance {
        let key = FunctionDeclKey::Function {
            name: decl_name.full_path.clone(),
            sig: decl_sig.clone(),
        };

        self.library.instantiate_func(&key)
    }

    pub fn build_closure_expr(&mut self, func: &typ::ast::AnonymousFunctionDef) -> ir::Ref {
        let closure = self
            .library
            .build_closure_instance(func);

        if func.captures.len() != 0 {
            return self.build_closure_value(closure, false);
        }

        let static_closure = self.library.build_static_closure(closure);
        let static_closure_ref = ir::Ref::from(static_closure.id);

        // closure objects have a specific type, but refs to closures are type erased so
        // we need to cast to Object. we know static closures are immortal, so this can be
        // a temp ref
        let closure_type = static_closure.identity.sig.to_closure_ptr_type();
        let closure_obj = self.local_temp(closure_type.clone());
        self.cast(closure_obj, static_closure_ref, closure_type);

        closure_obj.to_ref()
    }
    
    pub fn build_function_closure(&mut self, func: &FunctionInstance) -> ir::Ref {
        let static_closure = self
            .library
            .build_func_static_closure_instance(func);

        ir::Ref::Global(ir::GlobalRef::Variable(static_closure.id))
    }

    pub fn build_closure_value(&mut self, closure: ClosureInstance, immortal: bool) -> ir::Ref {
        let closure_def = self
            .library
            .metadata()
            .get_struct_def(closure.closure_id)
            .cloned()
            .unwrap();

        // this is a *specific* closure class, not a virtual (function typed) pointer
        let closure_ptr_ty = closure.closure_id.to_class_ptr_type([]);

        // virtual pointer to the closure
        let closure_virtual_ty = ir::Type::Object(ir::ObjectID::AnyClosure(closure.sig.clone()));
        let closure_virtual_ptr = self.local_var(closure_virtual_ty.clone(), None);

        self.scope(|builder| {
            let closure_ref = builder.local_temp(closure_ptr_ty.clone());
            builder.new_object(closure_ref, closure.closure_id, [], immortal);

            let func_field_ref = closure_ref.to_ref().field_ref(closure_ptr_ty.clone(), ir::CLOSURE_PTR_FIELD);

            // initialize closure reference to function
            let func_ref = ir::Ref::Global(ir::GlobalRef::func(closure.func_instance.id, []));
            // builder.cast(func_field_ptr.clone().to_deref(), func_ref, func_ptr_ty.clone());
            builder.mov(func_field_ref.to_deref(), func_ref);
            
            // initialize closure capture fields - copy from local scope into closure object
            for (field_id, field_def) in closure_def.fields.iter() {
                // skip the closure pointer field
                if *field_id == ir::CLOSURE_PTR_FIELD {
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

                if field_def.ty.contains_any_object_refs(builder.metadata()) {
                    builder.retain(capture_field_ref.to_deref(), field_def.ty.clone());
                };
            }
            
            builder.cast(closure_virtual_ptr, closure_ref, closure_virtual_ty.clone());
        });

        closure_virtual_ptr.to_ref()
    }
    
    pub fn get_method(&self, self_ty: &typ::Type, index: usize) -> typ::ast::MethodDecl {
        self.library.get_method(self_ty, index)
    }

    // for a given interface type and method index, get the method index from the self-type that
    // implements that method. panics on any error retrieving the methods, if the method index is
    // not valid for the interface type, or if the self-type is not an implementor of the interface
    pub fn get_impl_method_index<'a>(
        &self,
        self_ty: &'a typ::Type,
        iface_ty: &'a typ::Type,
        iface_method_index: usize,
    ) -> (&'a typ::Type, usize) {
        if self_ty == iface_ty {
            return (iface_ty, iface_method_index);
        }

        // a method invocation on a generic type is always translated as a virtual call - if
        // it's not an object type, the call will be devirtualized when the code is instantiated
        if let typ::Type::GenericParam(param) = self_ty {
            assert_eq!(param.is_ty.ty(), iface_ty);
            
            return (iface_ty, iface_method_index);
        }
        
        let iface_method = self.get_method(&iface_ty, iface_method_index);

        let method_name = iface_method.func_decl.ident();

        let impl_sig = iface_method.func_decl.sig().with_self(&self_ty);
        
        let method_index = self.library.find_method_index(self_ty, method_name, &impl_sig);
        (self_ty, method_index)
    }

    pub fn pretty_ty_name(&self, ty: &ir::Type) -> Cow<'_, str> {
        self.library.metadata().pretty_type_name(ty)
    }

    pub fn find_or_insert_string(&mut self, s: &str) -> ir::StringID {
        self.library.metadata_mut().find_or_insert_string(s)
    }

    #[expect(unused)]
    pub fn get_struct(&self, id: ir::TypeDefID) -> Option<&ir::StructDef> {
        self.library.metadata().get_struct_def(id)
    }

    #[expect(unused)]
    pub fn get_iface(&self, id: ir::InterfaceID) -> Option<&ir::InterfaceDef> {
        self.library.metadata().get_iface_def(id)
    }
    
    pub fn find_type_seq_support(&self, src_ty: &typ::Type) -> Option<TypeSequenceSupport> {
        self.library.find_type_seq_support(src_ty)
    }

    pub fn finish(mut self) -> ir::InstructionList {
        while self.local_stack.len() > 0 {
            self.local_end();
        }

        let var_count = self.local_stack.local_slot_count();
        let instruction_count = self.instructions.len() + var_count;

        let mut instructions = Vec::with_capacity(instruction_count);
        let mut sources = Vec::with_capacity(instruction_count);

        for (local_id, ty) in self.local_stack.finish() {
            instructions.push(ir::Instruction::LocalAlloc(local_id, ty));
            sources.push(None);
        }

        for instruction in self.instructions {
            instructions.push(instruction.instruction);
            sources.push(instruction.debug_span);
        }

        ir::InstructionList {
            instructions,
            sources,
        }
    }

    fn set_flags_ref(&mut self, set_ref: impl Into<ir::Ref>, set_type: &SetFlagsType) -> ir::Ref {
        let set_type = set_type.struct_id.to_struct_type([]);

        set_ref.into().field_ref(set_type, ir::FieldID(0))
    }
    
    pub fn set_include(
        &mut self,
        set_ref: impl Into<ir::Ref>,
        bit_val: impl Into<ir::Value>,
        set_type: &Arc<typ::SetDef>,
    ) {
        let flags_type_info = self.library.translate_set_type(set_type);
        let flags_ref = self.set_flags_ref(set_ref, &flags_type_info);

        self.call(ir::FunctionRef::new(flags_type_info.repr_type.include_func), [
            flags_ref.value(),
            bit_val.into(),
        ], None);
    }

    // todo: what is this for
    #[allow(unused)]
    pub fn set_exclude(
        &mut self,
        set_ref: impl Into<ir::Ref>,
        bit_val: impl Into<ir::Value>,
        set_type: &Arc<typ::SetDef>,
    ) {
        let flags_type_info = self.library.translate_set_type(set_type);
        let flags_ref = self.set_flags_ref(set_ref, &flags_type_info);

        self.call(ir::FunctionRef::new(flags_type_info.repr_type.exclude_func), [
            flags_ref.value(),
            bit_val.into(),
        ], None);
    }

    pub fn set_contains(&mut self,
        out: impl Into<ir::Ref>,
        set_ref: impl Into<ir::Ref>,
        bit_val: impl Into<ir::Value>,
        set_type: &Arc<typ::SetDef>
    ) {
        let flags_type_info = self.library.translate_set_type(set_type);
        let flags_ref = self.set_flags_ref(set_ref, &flags_type_info);

        self.call(ir::FunctionRef::new(flags_type_info.repr_type.contains_func), [
            flags_ref.value(),
            bit_val.into(),
        ], Some(out.into()));
    }
    
    pub fn set_eq(&mut self,
        out: impl Into<ir::Ref>,
        a: impl Into<ir::Ref>,
        b: impl Into<ir::Ref>,
        set_type: &Arc<typ::SetDef>,
    ) {
        let flags_type_info = self.library.translate_set_type(set_type);
        let a_ref = self.set_flags_ref(a, &flags_type_info);
        let b_ref = self.set_flags_ref(b, &flags_type_info);

        self.call(ir::FunctionRef::new(flags_type_info.repr_type.eq_func), [
            a_ref.value(),
            b_ref.value(),
        ], Some(out.into()));
    }

    pub fn set_bit_not(&mut self,
        a: impl Into<ir::Ref>,
        set_type: &Arc<typ::SetDef>,
    ) {
        let flags_type_info = self.library.translate_set_type(set_type);
        let a_ref = self.set_flags_ref(a, &flags_type_info);

        self.call(ir::FunctionRef::new(flags_type_info.repr_type.bit_not_func), [
            a_ref.value(),
        ], None);
    }

    fn set_bitwise_op(&mut self,
        a: impl Into<ir::Ref>,
        b: impl Into<ir::Ref>,
        set_type: &Arc<typ::SetDef>,
        get_func_id: fn(&SetFlagsType) -> ir::FunctionID,
    ) {
        let flags_type_info = self.library.translate_set_type(set_type);
        let a_ref = self.set_flags_ref(a, &flags_type_info);
        let b_ref = self.set_flags_ref(b, &flags_type_info);

        self.call(ir::FunctionRef::new(get_func_id(&flags_type_info)), [
            a_ref.value(),
            b_ref.value(),
        ], None);
    }

    pub fn set_bit_and(&mut self,
        a: impl Into<ir::Ref>,
        b: impl Into<ir::Ref>,
        set_type: &Arc<typ::SetDef>,
    ) {
        self.set_bitwise_op(a, b, set_type, |i| i.repr_type.bit_and_func)
    }

    pub fn set_bit_or(&mut self,
        a: impl Into<ir::Ref>,
        b: impl Into<ir::Ref>,
        set_type: &Arc<typ::SetDef>,
    ) {
        self.set_bitwise_op(a, b, set_type, |i| i.repr_type.bit_or_func)
    }

    pub fn set_bit_xor(&mut self,
        a: impl Into<ir::Ref>,
        b: impl Into<ir::Ref>,
        set_type: &Arc<typ::SetDef>,
    ) {
        self.set_bitwise_op(a, b, set_type, |i| i.repr_type.bit_xor_func)
    }

    #[expect(unused)]
    pub fn get_mem(&mut self, count: impl Into<ir::Value>, out: impl Into<ir::Ref>) {
        let function_ref = ir::Ref::Global(ir::GlobalRef::func(self.library.instantiate_get_mem_func(), []));
        self.call(function_ref, [count.into()], Some(out.into()));
    }

    #[expect(unused)]
    pub fn get_mem_id(&mut self) -> ir::FunctionID {
        self.library.instantiate_get_mem_func()
    }

    #[expect(unused)]
    pub fn free_mem(&mut self, at: impl Into<ir::Value>) {
        let function_ref = ir::Ref::Global(ir::GlobalRef::func(self.library.instantiate_free_mem_func(), []));
        self.call(function_ref, [at.into()], None);
    }

    pub fn bind_param(&mut self, id: ir::ArgID, ty: ir::Type, name: impl Into<String>) {
        self.local_stack_mut().bind_param(id, ty, Arc::new(name.into()), false)
    }

    pub fn bind_ref_param(&mut self, id: ir::ArgID, ty: ir::Type, name: impl Into<String>) {
        self.local_stack_mut().bind_param(id, ty.temp_ref(), Arc::new(name.into()), true)
    }

    // binds an anonymous return local in %0 with the indicated type
    pub fn bind_result(&mut self, ty: ir::Type) {
        self.local_stack_mut().bind_result(ty);
    }

    // binds an anonymous local binding for the closure pointer of a function
    pub fn bind_closure_ptr(&mut self, id: ir::ArgID, sig: &Rc<ir::FunctionSig>) {
        self.local_stack_mut().bind_unnamed_param(id, sig.to_closure_ptr_type(), false)
    }

    pub fn find_global_var(&self, name_path: &IdentPath) -> Option<ir::VariableID> {
        self.library.find_global_var(name_path)
    }

    pub fn local_closure_capture(&mut self, ty: ir::Type, name: String) -> ir::Ref {
        assert_ne!(ir::Type::Nothing, ty);

        let id = self.local_stack_mut().bind_capture(ty, name);

        id.to_ref()
    }

    pub fn begin_loop_body_scope(&mut self, continue_label: ir::Label, break_label: ir::Label) {
        self.local_stack_mut().push_loop(continue_label, break_label);
        self.local_begin();
    }

    pub fn end_loop_body_scope(&mut self) {
        self.local_end();
        self.local_stack_mut().pop_loop();
    }

    pub fn loop_body_scope<F>(
        &mut self,
        continue_label: ir::Label,
        break_label: ir::Label,
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
        length: impl Into<ir::Value>,
        index: impl Into<ir::Value>
    ) {
        self.comment("array bounds check");

        let index_val = index.into();
        let length_val = length.into();
        let bounds_ok_label = self.next_label();

        // if index >= 0 and index < arr.len then goto "bounds_ok"
        let gte_zero = self.gte_to_val(index_val.clone(), ir::Value::LiteralI32(0));
        let lt_len = self.lt_to_val(index_val, length_val);
        let bounds_check_ok = self.and_to_val(gte_zero, lt_len);

        self.jmpif(bounds_ok_label, bounds_check_ok);

        // otherwise: raise
        let err_str = self.find_or_insert_string("array index out of bounds");
        self.emit(ir::Instruction::Raise {
            val: ir::GlobalRef::StringLiteral(err_str).to_ref(),
        });

        self.label(bounds_ok_label);
    }

    pub fn exit_function(&mut self) {
        self.cleanup_scope(0);

        self.emit(ir::Instruction::Jump { dest: ir::EXIT_LABEL })
    }
}