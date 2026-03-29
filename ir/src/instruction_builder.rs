pub mod scope;
pub mod util;
mod dyn_array;
mod invoker;
mod object;

use crate::instruction_builder::object::gen_class_object_dtor_body;
use crate::instruction_builder::scope::ScopedBinding;
use crate::BinOpInstruction;
use crate::FunctionID;
use crate::FunctionSig;
use crate::IRFormatter;
use crate::Instruction;
use crate::InterfaceID;
use crate::Label;
use crate::LocalID;
use crate::MetadataBuilder;
use crate::MethodID;
use crate::ObjectID;
use crate::Ref;
use crate::Type;
use crate::TypeDefID;
use crate::UnaryOpInstruction;
use crate::Value;
use crate::{ArgID, FieldID};
use dyn_array::gen_dyn_array_dtor_body;
use dyn_array::new_array_from;
use scope::LocalStack;
use std::sync::Arc;
use terapascal_common::span::Span;

pub trait InstructionBuilder {
    fn emit(&mut self, instruction: Instruction);

    fn metadata(&self) -> &MetadataBuilder;
    fn metadata_mut(&mut self) -> &mut MetadataBuilder;

    fn local_stack(&self) -> &LocalStack;
    fn local_stack_mut(&mut self) -> &mut LocalStack;

    // if false, all comment instructions are skipped
    fn is_debug(&self) -> bool;

    fn ir_formatter(&self) -> &impl IRFormatter {
        self.metadata()
    }
    
    fn find_named(&self, name: &str) -> Option<&ScopedBinding> {
        self.local_stack().find_binding(name)
    }

    // creates an anonymous unmanaged local of this type
    fn local_temp(&mut self, ty: Type) -> LocalID {
        assert_ne!(Type::Nothing, ty);

        let id = self.local_stack_mut().bind_temp(ty.clone());

        id
    }

    // creates an autoreleased variable with the given name
    fn local_var(&mut self, ty: Type, name: Option<Arc<String>>) -> LocalID {
        assert_ne!(Type::Nothing, ty);
        
        let is_object = ty.is_object() || ty.is_weak();

        let id = match name {
            Some(name) => self.local_stack_mut().bind_var(ty, name),
            None => self.local_stack_mut().bind_auto_temp(ty),
        };

        if is_object {
            self.mov(id, Value::LiteralNil);
        }

        id
    }

    fn next_label(&mut self) -> Label;

    fn local_begin(&mut self) {
        let stack = self.local_stack_mut();

        stack.begin();
        let stack_len = stack.len();

        self.comment(&format!("begin scope {}", stack_len));
    }

    fn local_end(&mut self) {
        let top_scope = self.local_stack().len() - 1;
        self.cleanup_scope(top_scope);

        self.comment(&format!("end scope {}", top_scope));

        self.local_stack_mut().end();
    }

    fn push_source(&mut self, ctx: Span);
    fn pop_source(&mut self);

    /// release locals in all scopes after the position indicated by
    /// `to_scope` in the scope stack
    /// this should be used when jumping out a scope, or before popping one
    fn cleanup_scope(&mut self, to_scope: usize) {
        let last_scope = self.local_stack().len() - 1;
        assert!(
            to_scope <= last_scope,
            "cleanup_scope: index out of range: {}",
            to_scope
        );

        if self.is_debug() {
            if to_scope == last_scope {
                self.comment(&format!("cleanup scope {}", to_scope + 1));
            } else {
                self.comment(&format!(
                    "cleanup scopes {}..{}",
                    to_scope + 1,
                    last_scope + 1,
                ));
            }
        }

        let cleanup_range = to_scope..=last_scope;

        let bindings: Vec<_> = self
            .local_stack()
            .bindings_in_scope(cleanup_range)
            .cloned()
            .collect();

        // release local bindings that will be lost when the current scope is popped.
        // of course. releasing a ref should either insert a release instruction directly
        // (for an RC pointer) or insert a call to a structural release function (for
        // complex types containing RC pointers), so should never introduce new locals
        // in the scope being popped
        for binding in bindings {
            self.comment(&format!("expire {}", binding.to_ref()));
            self.expire_binding(&binding);
        }
    }

    fn expire_binding(&mut self, binding: &ScopedBinding) {
        if binding.auto_release {
            self.release_deep(binding.to_ref(), &binding.ty);
        }
    }

    fn break_loop(&mut self) {
        let (break_label, break_scope) = {
            let current_loop = self
                .local_stack()
                .current_loop()
                .expect("break stmt must appear in a loop");

            (current_loop.break_label, current_loop.block_level)
        };

        // write cleanup code for the broken scope and its children
        self.cleanup_scope(break_scope);

        // jump to the label (presumably somewhere outside the broken scope!)
        self.jmp(break_label);
    }

    fn continue_loop(&mut self) {
        let (continue_label, continue_scope) = {
            let current_loop = self
                .local_stack()
                .current_loop()
                .expect("continue stmt must appear in a loop");

            (current_loop.continue_label, current_loop.block_level)
        };

        self.cleanup_scope(continue_scope);
        self.jmp(continue_label);
    }

    fn comment(&mut self, content: impl Into<String>) {
        if !self.is_debug() {
            return;
        }

        self.emit(Instruction::Comment(content.into()));
    }

    fn mov(&mut self, out: impl Into<Ref>, val: impl Into<Value>) {
        self.emit(Instruction::Move {
            out: out.into(),
            new_val: val.into(),
        });
    }

    fn new_object(&mut self, out: impl Into<Ref>, type_id: TypeDefID, immortal: bool) {
        self.emit(Instruction::NewObject {
            out: out.into(),
            type_id,
            immortal,
        });
    }

    fn new_array(&mut self,
        out: impl Into<Ref>,
        element_type: Type,
        count: impl Into<Value>,
        immortal: bool,
    ) {
        self.emit(Instruction::NewArray {
            out: out.into(),
            element_type,
            count: count.into(),
            immortal,
        });
    }

    fn new_box(&mut self,
        out: impl Into<Ref>,
        value_type: Type,
        immortal: bool,
    ) {
        self.emit(Instruction::NewBox {
            out: out.into(),
            value_type,
            immortal,
        });
    }

    fn class_is(&mut self, out: impl Into<Ref>, a: impl Into<Value>, type_id: ObjectID) {
        self.emit(Instruction::ClassIs {
            out: out.into(),
            a: a.into(),
            class_id: type_id,
        })
    }

    fn add(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Add(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    fn add_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>, as_type: &Type) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            if let Some(result) = Value::from_literal_val(a_val + b_val, &as_type) {
                return result;
            }
        }

        let out = self.local_temp(as_type.clone());
        self.add(out.clone(), a, b);

        Value::from(out)
    }

    fn sub(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Sub(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    fn sub_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>, as_type: &Type) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            if let Some(result) = Value::from_literal_val(a_val - b_val, &as_type) {
                return result;
            }
        }

        let out = self.local_temp(as_type.clone());
        self.sub(out.clone(), a, b);

        Value::from(out)
    }

    fn mul(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Mul(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    fn mul_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>, as_type: &Type) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            if let Some(result) = Value::from_literal_val(a_val * b_val, &as_type) {
                return result;
            }
        }

        let out = self.local_temp(as_type.clone());
        self.mul(out.clone(), a, b);

        Value::from(out)
    }

    fn idiv(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::IDiv(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    fn fdiv(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::FDiv(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    fn modulo(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Mod(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    fn shl(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Shl(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    fn shr(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Shr(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
    }

    fn and(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::And(BinOpInstruction {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        }));
    }

    fn and_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        match (a.into(), b.into()) {
            (Value::LiteralBool(a), Value::LiteralBool(b)) => Value::LiteralBool(a && b),

            (a, b) => {
                let result = self.local_temp(Type::Bool);
                self.and(result.clone(), a, b);
                Value::from(result)
            },
        }
    }

    fn not_to_val(&mut self, bool_val: impl Into<Value>) -> Value {
        match bool_val.into() {
            Value::LiteralBool(b) => Value::LiteralBool(!b),

            other_val => {
                let result = self.local_temp(Type::Bool);
                self.not(result.clone(), other_val);
                Value::from(result)
            },
        }
    }

    fn not(&mut self, out: impl Into<Ref>, bool_val: impl Into<Value>) {
        self.emit(Instruction::Not(UnaryOpInstruction {
            a: bool_val.into(),
            out: out.into(),
        }));
    }

    fn or(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Or(BinOpInstruction {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        }));
    }

    fn or_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_bool(), b.to_literal_bool()) {
            Value::LiteralBool(a_val || b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.or(result.clone(), a, b);
            Value::from(result)
        }
    }

    fn bit_and(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::BitAnd(BinOpInstruction {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        }));
    }

    fn bit_not(&mut self, out: impl Into<Ref>, a: impl Into<Value>) {
        self.emit(Instruction::BitNot(UnaryOpInstruction {
            a: a.into(),
            out: out.into(),
        }));
    }

    fn bit_or(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::BitOr(BinOpInstruction {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        }));
    }

    fn bit_xor(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::BitXor(BinOpInstruction {
            a: a.into(),
            b: b.into(),
            out: out.into(),
        }));
    }

    fn eq(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Eq(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }))
    }

    fn neq(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        let out = out.into();
        
        self.eq(out.clone(), a, b);
        self.not(out.clone(), out);
    }

    fn eq_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val == b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.eq(result.clone(), a, b);
            Value::from(result)
        }
    }

    fn neq_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val != b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.neq(result.clone(), a, b);
            Value::from(result)
        }
    }

    fn gt(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Gt(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }))
    }

    fn gt_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val > b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.gt(result.clone(), a, b);
            Value::from(result)
        }
    }

    fn gte(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Gte(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }))
    }

    fn gte_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val >= b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.gte(result.clone(), a, b);
            Value::from(result)
        }
    }

    fn lt(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Lt(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }))
    }

    fn lte(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Lte(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }))
    }

    fn lt_to_val(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
        let a = a.into();
        let b = b.into();

        if let (Some(a_val), Some(b_val)) = (a.to_literal_val(), b.to_literal_val()) {
            Value::LiteralBool(a_val < b_val)
        } else {
            let result = self.local_temp(Type::Bool);
            self.lt(result.clone(), a, b);
            Value::from(result)
        }
    }

    fn size_of(&mut self, out: impl Into<Ref>, ty: Type) {
        self.mov(out, Value::SizeOf(ty));
    }

    #[deprecated]
    fn field(
        &mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        a_type: impl Into<Type>,
        field: FieldID,
    ) {
        let field_ref = a.into().field_ref(a_type.into(), field);
        self.mov(out, field_ref)
    }

    #[deprecated]
    fn element(
        &mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        index: impl Into<Value>,
        a_type: impl Into<Type>,
    ) {
        let element_ref = a.into().element_ref(a_type.into(), index);
        self.mov(out, element_ref)
    }

    fn element_val(
        &mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        index: impl Into<Value>,
        of_type: impl Into<Type>,
    ) {
        self.mov(out, a.into().element_ref(of_type.into(), index).to_deref());
    }

    fn element_to_val(
        &mut self,
        a: impl Into<Ref>,
        index: impl Into<Value>,
        element_ty: impl Into<Type>,
        of_type: impl Into<Type>,
    ) -> Ref {
        let element_ty = element_ty.into();
        let result = self.local_temp(element_ty.clone());
        self.element_val(result.clone(), a, index, of_type);

        Ref::Local(result)
    }

    fn length(&mut self, out: impl Into<Ref>, a: impl Into<Ref>, of_type: impl Into<Type>) {
        self.emit(Instruction::Length {
            out: out.into(),
            a: a.into(),
            of_type: of_type.into(),
        });
    }

    fn length_to_val(&mut self, a: impl Into<Ref>, of_type: impl Into<Type>) -> Value {
        let length_var = self.local_temp(Type::I32);
        self.length(length_var, a, of_type);

        length_var.value()
    }

    fn label(&mut self, label: Label) {
        self.emit(Instruction::Label(label))
    }

    fn jmp(&mut self, dest: Label) {
        self.emit(Instruction::Jump { dest })
    }

    fn jmpif(&mut self, dest: Label, cond: impl Into<Value>) {
        self.emit(Instruction::JumpIf {
            dest,
            test: cond.into(),
        })
    }

    fn call(
        &mut self,
        function: impl Into<Value>,
        args: impl IntoIterator<Item = Value>,
        out: Option<Ref>,
    ) {
        self.emit(Instruction::Call {
            function: function.into(),
            args: args.into_iter().collect(),
            out,
        })
    }

    fn vcall(
        &mut self,
        iface_id: InterfaceID,
        method: MethodID,
        self_arg: impl Into<Value>,
        rest_args: impl IntoIterator<Item = Value>,
        out: Option<Ref>,
    ) {
        self.emit(Instruction::VirtualCall {
            iface_id,
            method,
            self_arg: self_arg.into(),
            rest_args: rest_args.into_iter().map(|arg| arg.into()).collect(),
            out,
        })
    }

    fn addr_of(&mut self, out: impl Into<Ref>, a: impl Into<Ref>) {
        self.emit(Instruction::AddrOf {
            out: out.into(),
            a: a.into(),
        })
    }

    fn make_ref(&mut self, out: impl Into<Ref>, a: impl Into<Ref>) {
        self.emit(Instruction::MakeRef {
            out: out.into(),
            a: a.into(),
        })
    }

    fn make_ref_local(&mut self, a: impl Into<Ref>, ty: &Type) -> LocalID {
        let local = self.local_temp(ty.clone().temp_ref());
        self.make_ref(local, a);
        
        local
    }

    fn cast(&mut self, out: impl Into<Ref>, val: impl Into<Value>, ty: Type) {
        self.emit(Instruction::Cast {
            out: out.into(),
            a: val.into(),
            ty,
        })
    }

    #[deprecated]
    fn vartag(
        &mut self,
        out: impl Into<Ref>,
        instance: impl Into<Ref>,
        instance_type: Type,
    ) {
        self.mov(out, instance.into().vartag_ref(instance_type));
    }

    #[deprecated]
    fn vardata(
        &mut self,
        out: impl Into<Ref>,
        instance: impl Into<Ref>,
        instance_type: Type,
        tag: usize,
    ) {
        self.mov(out, instance.into().vardata_ref(instance_type, tag));
    }

    fn release(&mut self, at: impl Into<Ref>, weak: bool, released_out: impl Into<Ref>) {
        self.emit(Instruction::Release {
            at: at.into(),
            weak,
            released_out: released_out.into(),
        });
    }

    fn retain(&mut self, at: impl Into<Ref>, weak: bool) -> bool {
        self.emit(Instruction::Retain {
            at: at.into(),
            weak,
        });

        true
    }

    fn release_deep(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        let at = at.into();
        if at.is_discard() {
            panic!("release_deep: operand must not be a discard ref");
        }

        self.visit_deep(
            at,
            ty,
            |builder, element_ty, element_ref| {
                if !element_ty.is_object() {
                    return false;
                }
                
                builder.comment(format!(
                    "release: {}",
                    ty.to_pretty_string(builder.ir_formatter())
                ));

                builder.release(element_ref, element_ty.is_weak(), Ref::Discard);
                true
            },
        )
    }

    fn retain_deep(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        let at = at.into();
        if at.is_discard() {
            panic!("retain_deep: operand must not be a discard ref");
        }
        
        self.visit_deep(
            at,
            ty,
            |builder, element_ty, element_ref| {
                if !element_ty.is_object() {
                    return false;
                }

                builder.comment(format!(
                    "retain: {}",
                    ty.to_pretty_string(builder.ir_formatter())
                ));
                
                builder.retain(element_ref, element_ty.is_weak());
                true
            },
        )
    }

    fn ref_to_ptr_val(&mut self, at: impl Into<Ref>, ty: &Type) -> Value {
        let at = at.into();

        if let Ref::Deref(at_ptr) = &at {
            return (**at_ptr).clone();
        }

        let temp_at_ptr = self.local_temp(ty.clone().temp_ref());
        self.make_ref(temp_at_ptr.clone(), at);
        temp_at_ptr.value()
    }
    
    fn gen_class_object_dtor_body(&mut self, class_id: TypeDefID, self_param: ArgID) -> bool {
        gen_class_object_dtor_body(self, class_id, self_param)
    }

    fn gen_dyn_array_dtor_body(&mut self, self_param: ArgID, element_type: &Type) {
        gen_dyn_array_dtor_body(self, self_param, element_type)
    }

    /// Builds a dynamic array from the given elements
    fn new_array_from(
        &mut self,
        elements: impl IntoIterator<Item = Value>,
        element_type: &Type,
    ) -> Ref
    where
        Self: Sized,
    {
        new_array_from(self, elements, element_type)
    }

    fn if_then<Branch>(&mut self, cond: impl Into<Value>, then_branch: Branch)
    where
        Branch: FnOnce(&mut Self),
    {
        util::if_then(self, cond, then_branch);
    }

    fn if_then_else<IfBranch, ElseBranch>(
        &mut self,
        cond: impl Into<Value>,
        then_branch: IfBranch,
        else_branch: ElseBranch,
    ) where
        IfBranch: FnOnce(&mut Self),
        ElseBranch: FnOnce(&mut Self),
    {
        util::if_then_else(self, cond, then_branch, else_branch);
    }

    fn counter_loop<F>(
        &mut self,
        counter: impl Into<Ref>,
        inc_val: impl Into<Value>,
        high_val: impl Into<Value>,
        f: F,
    ) where
        F: FnOnce(&mut Self),
    {
        util::counter_loop(self, counter, inc_val, high_val, f);
    }
    
    fn gen_default_init(&mut self, at: impl Into<Ref>, ty: &Type) {
        util::gen_default_init(self, at, ty)
    }

    // inline IR for FillByte-style memory set procedure
    fn gen_fill_byte(&mut self, at: Ref, count: Value, byte_val: Value) {
        util::gen_fill_byte(self, at, count, byte_val);
    }
    
    fn gen_invoker_body(&mut self,
        func_id: FunctionID,
        func_sig: &FunctionSig,
        self_ref: Ref,
        args_ref: Ref,
        error_out_ref: Ref,
    ) {
        invoker::gen_invoker_body(self, func_id, func_sig, self_ref, args_ref, error_out_ref);
    }

    fn while_do<CondFn, DoFn>(&mut self, cond_fn: CondFn, do_fn: DoFn)
    where
        CondFn: Fn(&mut Self, Ref),
        DoFn: Fn(&mut Self, Label, Label),
    {
        util::while_do(self, cond_fn, do_fn);
    }

    /// Call `f` for every structural member of the object of type `ty_def` found at the `at`
    /// reference.
    ///
    /// Each call to the visitor should return a value indicating whether the reference 
    /// was used. If the visitor did nothing, we can prune the field local used to access it.
    /// This assumes that false indicates that no new instructions were added and may misbehave
    /// otherwise.
    fn visit_deep<Visitor>(&mut self, at: impl Into<Ref>, ty: &Type, f: Visitor) -> bool
    where
        Visitor: Fn(&mut Self, &Type, Ref) -> bool + Copy,
    {
        util::visit_deep(self, at, ty, f)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct LocalBinding {
    pub id: LocalID,
    pub by_ref: bool,
}

impl LocalBinding {
    pub fn to_ref(&self) -> Ref {
        if self.by_ref {
            self.id.to_deref()
        } else {
            self.id.to_ref()
        }
    }
}
