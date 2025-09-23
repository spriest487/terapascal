pub mod scope;

use crate::BinOpInstruction;
use crate::FieldID;
use crate::IRFormatter;
use crate::Instruction;
use crate::InterfaceID;
use crate::Label;
use crate::LocalID;
use crate::Metadata;
use crate::MethodID;
use crate::Ref;
use crate::Type;
use crate::TypeDefID;
use crate::UnaryOpInstruction;
use crate::Value;
use crate::VirtualTypeID;
use std::fmt;

pub trait InstructionBuilder {
    fn emit(&mut self, instruction: Instruction);
    
    fn metadata(&self) -> &Metadata;
    
    // if false, all comment instructions are skipped
    fn is_debug(&self) -> bool;
    
    fn ir_formatter(&self) -> &impl IRFormatter;

    // creates an anonymous unmanaged local of this type
    fn local_temp(&mut self, ty: Type) -> LocalID;
    fn next_label(&mut self) -> Label;
    
    fn local_begin(&mut self);
    fn local_end(&mut self);

    fn comment(&mut self, content: &(impl fmt::Display + ?Sized)) {
        if !self.is_debug() {
            return;
        }

        self.emit(Instruction::Comment(content.to_string()));
    }

    fn mov(&mut self, out: impl Into<Ref>, val: impl Into<Value>) {
        self.emit(Instruction::Move {
            out: out.into(),
            new_val: val.into(),
        });
    }

    fn rc_new(&mut self, out: impl Into<Ref>, type_id: TypeDefID, immortal: bool) {
        self.emit(Instruction::RcNew {
            out: out.into(),
            type_id,
            immortal,
        });
    }

    fn class_is(&mut self, out: impl Into<Ref>, a: impl Into<Value>, type_id: VirtualTypeID) {
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

    fn or_to_value(&mut self, a: impl Into<Value>, b: impl Into<Value>) -> Value {
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

    fn field(
        &mut self,
        out: impl Into<Ref>,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
    ) {
        self.emit(Instruction::Field {
            out: out.into(),
            a: base.into(),
            of_ty: base_ty.into(),
            field,
        })
    }

    fn field_val(
        &mut self,
        out: impl Into<Ref>,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
        field_ty: Type,
    ) {
        let field_ptr = self.local_temp(field_ty.ptr());
        self.field(field_ptr.clone(), base, base_ty, field);

        self.mov(out, Ref::from(field_ptr).to_deref());
    }

    fn field_to_val(
        &mut self,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
        field_ty: Type
    ) -> Ref {
        let result = self.local_temp(field_ty.clone());
        self.field_val(result.clone(), base, base_ty, field, field_ty);

        Ref::Local(result)
    }

    fn assign_field(
        &mut self,
        base: impl Into<Ref>,
        base_ty: impl Into<Type>,
        field: FieldID,
        field_ty: Type,
        val: impl Into<Value>,
    ) {
        let field_ptr = self.local_temp(field_ty.ptr());
        self.field(field_ptr.clone(), base, base_ty, field);

        self.mov(Ref::Local(field_ptr).to_deref(), val);
    }

    fn element(
        &mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        index: impl Into<Value>,
        element_ty: impl Into<Type>,
    ) {
        self.emit(Instruction::Element {
            element: element_ty.into(),
            out: out.into(),
            a: a.into(),
            index: index.into(),
        });
    }

    fn element_val(
        &mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        index: impl Into<Value>,
        element_ty: impl Into<Type>,
    ) {
        let element_ty = element_ty.into();
        let element_ptr = self.local_temp(element_ty.clone().ptr());

        self.element(element_ptr.clone(), a, index, element_ty);
        self.mov(out, Ref::Local(element_ptr).to_deref());
    }

    #[allow(unused)]
    fn element_to_val(
        &mut self,
        a: impl Into<Ref>,
        index: impl Into<Value>,
        element_ty: impl Into<Type>,
    ) -> Ref {
        let element_ty = element_ty.into();
        let result = self.local_temp(element_ty.clone());
        self.element_val(result.clone(), a, index, element_ty);

        Ref::Local(result)
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
        rest_args: impl IntoIterator<Item=impl Into<Value>>,
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

    fn cast(&mut self, out: impl Into<Ref>, val: impl Into<Value>, ty: Type) {
        self.emit(Instruction::Cast {
            out: out.into(),
            a: val.into(),
            ty,
        })
    }

    fn vartag(&mut self, out: impl Into<Ref>, a: impl Into<Ref>, of_ty: Type) {
        self.emit(Instruction::VariantTag {
            out: out.into(),
            a: a.into(),
            of_ty,
        })
    }

    fn vardata(
        &mut self,
        out: impl Into<Ref>,
        a: impl Into<Ref>,
        of_ty: Type,
        tag: usize,
    ) {
        self.emit(Instruction::VariantData {
            out: out.into(),
            a: a.into(),
            of_ty,
            tag,
        })
    }

    fn release(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        let at = at.into();

        self.comment(&format!("release: {}", ty.to_pretty_string(self.ir_formatter())));

        self.release_deep(at, ty)
    }
    
    fn release_deep(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        self.visit_deep(at, ty, |builder, element_ty, element_ref| {
            match element_ty {
                Type::RcPointer(..) => {
                    builder.emit(Instruction::Release { at: element_ref, weak: false });
                    true
                }

                Type::RcWeakPointer(..) => {
                    builder.emit(Instruction::Release { at: element_ref, weak: true });
                    true
                }
                
                _ => false,
            }
        })
    }

    fn retain(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        let at = at.into();

        self.comment(&format!("retain: {}", ty.to_pretty_string(self.ir_formatter())));

        self.retain_deep(at, ty)
    }
    
    fn retain_deep(&mut self, at: impl Into<Ref>, ty: &Type) -> bool {
        self.visit_deep(at, ty, |builder, element_ty, element_ref| {
            match element_ty {
                Type::RcPointer(..) => {
                    builder.emit(Instruction::Retain { at: element_ref, weak: false });
                    true
                }

                Type::RcWeakPointer(..) => {
                    builder.emit(Instruction::Retain { at: element_ref, weak: true });
                    true
                }

                _ => false,
            }
        })
    }
    
    fn if_then<Branch>(&mut self, cond: impl Into<crate::Value>, then_branch: Branch)
    where
        Branch: FnOnce(&mut Self),
    {
        let then_label = self.next_label();
        let break_label = self.next_label();

        self.jmpif(then_label, cond);
        self.jmp(break_label);

        self.label(then_label);
        then_branch(self);

        self.label(break_label);
    }

    fn if_then_else<Branch>(&mut self,
        cond: impl Into<crate::Value>,
        then_branch: Branch,
        else_branch: Branch,
    )
    where
        Branch: FnOnce(&mut Self)
    {
        let then_label = self.next_label();
        let else_label = self.next_label();
        let break_label = self.next_label();

        self.jmpif(then_label, cond);
        self.jmp(else_label);

        self.label(then_label);
        then_branch(self);
        self.jmp(break_label);

        self.label(else_label);
        else_branch(self);

        self.label(break_label);
    }

    fn counter_loop<F>(
        &mut self,
        counter: impl Into<crate::Ref>,
        inc_val: impl Into<crate::Value>,
        high_val: impl Into<crate::Value>,
        f: F,
    )
    where F: Fn(&mut Self)
    {
        let break_label = self.next_label();
        let loop_label = self.next_label();
        let done = self.local_temp(crate::Type::Bool);

        let counter = counter.into();

        self.label(loop_label);
        self.gte(done.clone(), counter.clone(), high_val);
        self.jmpif(break_label, done);

        f(self);

        self.add(counter.clone(), counter, inc_val);
        self.jmp(loop_label);
        self.label(break_label);
    }

    /// call `f` for every structural member of the object of type `ty_def` found at the `at`
    /// reference.
    ///
    /// returns `true` if calling `f` for any of the members (or members of members) returns `true`
    fn visit_deep<Visitor>(&mut self, at: impl Into<Ref>, ty: &Type, f: Visitor) -> bool
    where
        Visitor: Fn(&mut Self, &Type, Ref) -> bool + Copy,
    {
        let at = at.into();
        
        match ty {
            Type::Struct(struct_id) => {
                let struct_def = self.metadata().get_struct_def(*struct_id).unwrap();

                let fields: Vec<_> = struct_def
                    .fields
                    .iter()
                    .map(|(field_id, field)| (*field_id, field.ty.clone()))
                    .collect();

                let mut result = false;
                for (field, field_ty) in fields {
                    if !(field_ty.is_rc() || field_ty.is_complex()) {
                        continue;
                    }

                    // store the field pointer in a temp slot
                    let field_val = Ref::Local(self.local_temp(field_ty.clone().ptr()));

                    let of_ty = Type::Struct(*struct_id);
                    self.field(field_val.clone(), at.clone(), of_ty, field);

                    result |= self.visit_deep(field_val.to_deref(), &field_ty, f);
                }

                result
            },

            Type::Variant(id) => {
                let cases = &self
                    .metadata()
                    .get_variant_def(*id)
                    .unwrap_or_else(|| panic!("missing variant def {}", id))
                    .cases
                    .to_vec();

                let tag_ptr = Ref::Local(self.local_temp(Type::I32.ptr()));
                let is_not_case = Ref::Local(self.local_temp(Type::Bool));

                // get the tag
                self.vartag(tag_ptr.clone(), at.clone(), Type::Variant(*id));

                // jump out of the search loop if we find the matching case
                let break_label = self.next_label();

                let mut result = false;

                // for each case, check if the tag matches and jump past it if not

                for (tag, case) in cases.iter().enumerate() {
                    self.comment(&format!("testing for variant case {} ({})", tag, case.name));

                    if let Some(data_ty) = &case.ty {
                        if !(data_ty.is_rc() || data_ty.is_complex()) {
                            continue;
                        }

                        let skip_case_label = self.next_label();

                        // is_not_case := tag_ptr^ != tag
                        let tag_val = Value::LiteralI32(tag as i32);
                        self.eq(is_not_case.clone(), tag_ptr.clone().to_deref(), tag_val);
                        self.not(is_not_case.clone(), is_not_case.clone());

                        self.jmpif(skip_case_label, is_not_case.clone());

                        // get ptr into case data and visit it

                        // only one data_ptr local will be allocated depending on which case is
                        // active, so a scope is needed here to stop the local counter being
                        // incremented once per case
                        self.local_begin();
                        let data_ptr = self.local_temp(data_ty.clone().ptr());
                            
                        self.vardata(
                            data_ptr.clone(),
                            at.clone(),
                            Type::Variant(*id),
                            tag,
                        );

                        result |= self.visit_deep(data_ptr.to_ref().to_deref(), &data_ty, f);
                        self.local_end();

                        // break after any case executes
                        self.jmp(break_label);

                        // jump to here if this case isn't active
                        self.label(skip_case_label);
                    }
                }

                self.label(break_label);

                result
            },

            Type::Array { element, dim } => {
                if !element.is_rc() && !element.is_complex() {
                    return false;
                }

                let element_ty = (**element).clone();
                let element_ptr = Ref::Local(self.local_temp(element_ty.clone().ptr()));
                let mut result = false;

                for i in 0..*dim {
                    let index = Value::LiteralI32(i as i32);
                    self.element(element_ptr.clone(), at.clone(), index, element_ty.clone());

                    result |= self.visit_deep(element_ptr.clone().to_deref(), element, f);
                }

                result
            },

            // field or element
            _ => f(self, ty, at),
        }
    }
}

pub fn remove_empty_blocks(instructions: &mut Vec<Instruction>) {
    let mut pc = 0;

    // stack of instruction indices at which the current empty scope begins
    let mut empty = vec![Some(pc)];

    while pc < instructions.len() {
        let block_empty = empty.last_mut().unwrap();

        match &instructions[pc] {
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
                        instructions.remove(pc);
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

pub fn jmp_exists(instructions: &[Instruction], to_label: Label) -> bool {
    instructions.iter().any(|i| match i {
        Instruction::Jump { dest } | Instruction::JumpIf { dest, .. } => *dest == to_label,
        _ => false,
    })
}
