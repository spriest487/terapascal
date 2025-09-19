use crate::BinOpInstruction;
use crate::FieldID;
use crate::Instruction;
use crate::InterfaceID;
use crate::Label;
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
    
    // if false, all comment instructions are skipped
    fn is_debug(&self) -> bool;

    // creates an anonymous unmanaged local of this type
    fn local_temp(&mut self, ty: Type) -> Ref;

    // creates a GC-managed local of this type, with an optional name
    fn local_new(&mut self, ty: Type, name: Option<String>) -> Ref;

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

        Value::Ref(out)
    }

    fn mul(&mut self, out: impl Into<Ref>, a: impl Into<Value>, b: impl Into<Value>) {
        self.emit(Instruction::Mul(BinOpInstruction {
            out: out.into(),
            a: a.into(),
            b: b.into(),
        }));
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
                Value::Ref(result)
            },
        }
    }

    fn not_to_val(&mut self, bool_val: impl Into<Value>) -> Value {
        match bool_val.into() {
            Value::LiteralBool(b) => Value::LiteralBool(!b),

            other_val => {
                let result = self.local_temp(Type::Bool);
                self.not(result.clone(), other_val);
                Value::Ref(result)
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
            Value::Ref(result)
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
            Value::Ref(result)
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
            Value::Ref(result)
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
            Value::Ref(result)
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
            Value::Ref(result)
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

        self.mov(out, field_ptr.to_deref());
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

        result
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

        self.mov(field_ptr.to_deref(), val);
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
        self.mov(out, element_ptr.to_deref());
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

        result
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
}
