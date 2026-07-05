use crate::instruction::BinOpInstruction;
use crate::instruction::Instruction;
use crate::metadata::MethodID;
use crate::types::Type;
use crate::val::Ref;
use crate::val::Value;
use crate::DeclPath;
use crate::FieldID;
use crate::FunctionRef;
use crate::InterfaceRef;
use crate::NamePath;
use crate::TypeDefID;
use crate::TypeParam;
use crate::UnaryOpInstruction;
use std::fmt;
use std::write;

const IX_WIDTH: usize = 8;

pub trait IRFormatter {
    fn format_instruction(
        &self,
        instruction: &Instruction,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        match instruction {
            Instruction::Comment(comment) => write!(f, "{:>width$} {}", "//", comment, width = IX_WIDTH),

            Instruction::LocalAlloc(id, ty) => {
                write!(f, "{:>width$} ", "local", width = IX_WIDTH)?;
                self.format_ref(&Ref::Local(*id), f)?;
                write!(f, " of ")?;
                self.format_type(ty, f)
            }

            Instruction::Move { out, new_val } => {
                write!(f, "{:>width$} ", "mov", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(new_val, f)
            }
            Instruction::Add(op) => {
                self.format_bin_op_instruction(f, "add", op, "+")
            }
            Instruction::Sub(op) => {
                self.format_bin_op_instruction(f, "sub", op, "-")
            }
            Instruction::Mul(op) => {
                self.format_bin_op_instruction(f, "mul", op, "*")
            }
            Instruction::IDiv(op) => {
                self.format_bin_op_instruction(f, "idiv", op, "div")
            }
            Instruction::FDiv(op) => {
                self.format_bin_op_instruction(f, "fdiv", op, "/")
            }
            Instruction::Mod(op) => {
                self.format_bin_op_instruction(f, "mod", op, "mod")
            }
            Instruction::Shl(op) => {
                self.format_bin_op_instruction(f, "shl", op, "shl")
            }
            Instruction::Shr(op) => {
                self.format_bin_op_instruction(f, "shr", op, "shr")
            }

            Instruction::BitAnd(op) => {
                self.format_bin_op_instruction(f, "bitand", op, "&")
            }
            Instruction::BitOr(op) => {
                self.format_bin_op_instruction(f, "bitor", op, "|")
            }
            Instruction::BitXor(op) => {
                self.format_bin_op_instruction(f, "bixor", op, "^")
            }
            Instruction::BitNot(op) => {
                self.format_prefix_op_instruction(f, "bitnot", op, "~")
            }

            Instruction::Eq(op) => {
                self.format_bin_op_instruction(f, "eq", op, "=")
            }

            Instruction::Gt(op) => {
                self.format_bin_op_instruction(f, "gt", op, ">")
            }
            Instruction::Gte(op) => {
                self.format_bin_op_instruction(f, "gte", op, ">=")
            }
            Instruction::Lt(op) => {
                self.format_bin_op_instruction(f, "lt", op, "<")
            }
            Instruction::Lte(op) => {
                self.format_bin_op_instruction(f, "lte", op, "<=")
            }

            Instruction::Not(op) => {
                self.format_prefix_op_instruction(f, "not", op, "not ")
            }

            Instruction::And(op) => {
                self.format_bin_op_instruction(f, "and", op, "and")
            }
            Instruction::Or(op) => {
                self.format_bin_op_instruction(f, "or", op, "or")
            }

            Instruction::Call {
                out,
                function,
                args,
            } => {
                write!(f, "{:>width$} ", "call", width = IX_WIDTH)?;
                if let Some(out) = out {
                    self.format_ref(out, f)?;
                    write!(f, " := ")?;
                }

                self.format_val(function, f)?;

                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    self.format_val(arg, f)?;
                }
                write!(f, ")")
            }

            Instruction::VirtualCall {
                out,
                iface_ref,
                method,
                self_arg,
                rest_args,
            } => {
                write!(f, "{:>width$} ", "vcall", width = IX_WIDTH)?;

                if let Some(out) = out {
                    self.format_ref(out, f)?;
                    write!(f, " := ")?;
                }
                write!(f, "(")?;

                self.format_ref(self_arg, f)?;
                write!(f, " as ")?;
                self.format_type(&iface_ref.to_object_id().to_object_type(), f)?;

                write!(f, ").")?;
                self.format_iface_method(iface_ref, *method, f)?;
                write!(f, "(")?;

                for (i, arg) in rest_args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    self.format_val(arg, f)?;
                }
                write!(f, ")")
            }

            Instruction::IsType { out, a, value_type, is_type } => {
                write!(f, "{:>width$} ", "is", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " (")?;
                self.format_type(value_type, f)?;
                write!(f, ") is ")?;
                self.format_type(is_type, f)
            }

            Instruction::AddrOf { out, a } => {
                write!(f, "{:>width$} ", "addrof", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := @")?;
                self.format_ref(a, f)
            }
            Instruction::MakeRef { out, a } => {
                write!(f, "{:>width$} ", "makeref", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := &")?;
                self.format_ref(a, f)
            }
            
            Instruction::Length {
                out,
                a,
                of_type,
            } => {
                write!(f, "{:>width$} ", "length", width = IX_WIDTH)?;
                self.format_ref(out, f)?;
                write!(f, " := length of ")?;

                self.format_ref(a, f)?;
                write!(f, " as ")?;
                self.format_type(of_type, f)
            }

            Instruction::Label(label) => {
                write!(f, "{:>width$} {}", "label", label, width = IX_WIDTH)
            }

            Instruction::Jump { dest } => {
                write!(f, "{:>width$} {}", "jmp", dest, width = IX_WIDTH)
            },

            Instruction::JumpIf { dest, test } => {
                write!(f, "{:>width$} {} if ", "jmpif", dest, width = IX_WIDTH)?;
                self.format_val(test, f)
            }

            Instruction::NewObject { out, type_id, type_args, immortal } => {
                write!(f, "{:>width$} ", "new", width = IX_WIDTH)?;
                self.format_type(&type_id.to_struct_type(type_args.clone()), f)?;

                write!(f, " at ")?;
                self.format_ref(out, f)?;
                
                if *immortal {
                    write!(f, " (immortal)")?;
                }
                Ok(())
            }
            
            Instruction::NewArray { out, element_type, count, immortal } => {
                write!(f, "{:>width$} ", "newarr", width = IX_WIDTH)?;
                
                self.format_ref(out, f)?;

                write!(f, " := [")?;
                self.format_type(element_type, f)?;
                write!(f, ", ")?;
                self.format_val(count, f)?;
                write!(f, "]")?;

                if *immortal {
                    write!(f, " (immortal)")?;
                }
                
                Ok(())
            }

            Instruction::NewBox { out, value_type, immortal } => {
                write!(f, "{:>width$} ", "newbox", width = IX_WIDTH)?;

                self.format_ref(out, f)?;

                write!(f, " := [")?;
                self.format_type(value_type, f)?;
                write!(f, "]")?;

                if *immortal {
                    write!(f, " (immortal)")?;
                }

                Ok(())
            }

            Instruction::Release { at, value_type, released_out } => {
                write!(f, "{:>width$} ", "release", width = IX_WIDTH)?;
                
                if *released_out != Ref::Discard {
                    self.format_ref(released_out, f)?;
                    write!(f, " := ")?;
                }

                self.format_ref(at, f)?;

                write!(f, " (-1 ")?;
                self.format_type(value_type, f)?;
                write!(f, ")")?;

                Ok(())
            }

            Instruction::Retain { at, value_type } => {
                write!(f, "{:>width$} ", "retain", width = IX_WIDTH)?;

                self.format_ref(at, f)?;

                write!(f, " (+1 ")?;
                self.format_type(value_type, f)?;
                write!(f, ")")?;

                Ok(())
            }

            Instruction::Raise { val } => {
                write!(f, "{:>width$} ", "raise", width = IX_WIDTH)?;
                self.format_ref(val, f)
            }

            Instruction::Cast { out, ty, a } => {
                write!(f, "{:>width$} ", "cast", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;

                write!(f, " as ")?;
                self.format_type(ty, f)
            }
        }
    }

    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_type_def(&self, id: TypeDefID, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_func_ref(&self, r: &FunctionRef, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result;

    fn format_iface_method(
        &self,
        iface: &InterfaceRef,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result;

    fn format_type_def_method(
        &self,
        def_id: TypeDefID,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result;

    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut dyn fmt::Write) -> fmt::Result;
    
    fn format_bin_op_instruction(&self,
        f: &mut dyn fmt::Write,
        instruction_name: &str,
        op: &BinOpInstruction,
        operator: &str
    ) -> fmt::Result {
        write!(f, "{:>width$} ", instruction_name, width = IX_WIDTH)?;

        self.format_ref(&op.out, f)?;
        write!(f, " := ")?;

        self.format_val(&op.a, f)?;
        write!(f, " {} ", operator)?;
        self.format_val(&op.b, f)?;
        
        Ok(())
    }

    fn format_prefix_op_instruction(
        &self,
        f: &mut dyn fmt::Write,
        instruction_name: &str,
        op: &UnaryOpInstruction,
        operator: &str
    ) -> fmt::Result {
        write!(f, "{:>width$} ", instruction_name, width = IX_WIDTH)?;

        self.format_ref(&op.out, f)?;
        write!(f, " := {}", operator)?;
        self.format_val(&op.a, f)?;

        Ok(())
    }

    fn format_decl(&self, name: &DeclPath, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", name.path.join("."))?;

        if !name.type_params.is_empty() {
            self.format_type_params(&name.type_params, f)?;
        }
        Ok(())
    }

    fn format_name(&self, name: &NamePath, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", name.path.join("."))?;

        if !name.type_args.is_empty() {
            self.format_type_args(&name.type_args, f)?;
        }
        Ok(())
    }

    fn format_type_params(&self, params: &[TypeParam], f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "[")?;
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", param.name)?;

            if let Some(constraint) = &param.constraint {
                write!(f, " is ")?;
                self.format_type(constraint, f)?;
            }
        }
        write!(f, "]")
    }
    
    fn format_type_args(&self, args: &[Type], f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "[")?;
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }

            self.format_type(arg, f)?;
        }
        write!(f, "]")
    }
}

pub struct RawFormatter;

impl IRFormatter for RawFormatter {
    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", ty)
    }

    fn format_type_def(&self, id: TypeDefID, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{{type {}}}", id)
    }

    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", val)
    }

    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", r)
    }

    fn format_func_ref(&self, r: &FunctionRef, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", r.def_id)?;

        if !r.args.is_empty() {
            self.format_type_args(&r.args, f)?;
        }
        
        Ok(())
    }

    fn format_field(&self, _of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", field)
    }

    fn format_iface_method(
        &self,
        _iface_id: &InterfaceRef,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        write!(f, "<method {}>", method.0)
    }

    fn format_type_def_method(&self,
        _def_id: TypeDefID,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        write!(f, "<method {}>", method.0)
    }

    fn format_variant_case(
        &self,
        _of_ty: &Type,
        tag: usize,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        write!(f, "data_{}", tag)
    }
}