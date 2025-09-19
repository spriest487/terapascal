use crate::instruction::BinOpInstruction;
use crate::instruction::Instruction;
use crate::metadata::MethodID;
use crate::ty::FieldID;
use crate::ty::Type;
use crate::ty::VirtualTypeID;
use crate::ty_decl::InterfaceID;
use crate::val::Ref;
use crate::val::Value;
use crate::NamePath;
use crate::UnaryOpInstruction;
use std::cell::Cell;
use std::fmt;

const IX_WIDTH: usize = 8;

pub trait IRFormatter {
    fn format_instruction<W: fmt::Write>(
        &self,
        instruction: &Instruction,
        f: &mut W,
    ) -> fmt::Result {
        match instruction {
            Instruction::Comment(comment) => write!(f, "{:>width$} {}", "//", comment, width = IX_WIDTH),

            Instruction::DebugPush(span) => {
                write!(f, "{:>width$} {}", "dbgpush", span, width = IX_WIDTH)
            }

            Instruction::DebugPop => {
                write!(f, "{:>width$}", "dbgpop", width = IX_WIDTH)
            }

            Instruction::LocalAlloc(id, ty) => {
                write!(f, "{:>width$} ", "local", width = IX_WIDTH)?;
                self.format_ref(&Ref::Local(*id), f)?;
                write!(f, " of ")?;
                self.format_type(ty, f)
            }
            Instruction::LocalBegin => write!(f, "{:>width$} ", "begin", width = IX_WIDTH),
            Instruction::LocalEnd => write!(f, "{:>width$} ", "end", width = IX_WIDTH),

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
                self.format_prefix_op_instruction(f, "not", op, "not")
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
                iface_id,
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

                self.format_val(self_arg, f)?;
                write!(f, " as ")?;
                self.format_type(&Type::RcPointer(VirtualTypeID::Interface(*iface_id)), f)?;

                write!(f, ").")?;
                self.format_method(*iface_id, *method, f)?;
                write!(f, "(")?;

                for (i, arg) in rest_args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    self.format_val(arg, f)?;
                }
                write!(f, ")")
            }

            Instruction::ClassIs { out, a, class_id } => {
                write!(f, "{:>width$} ", "is", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := ")?;
                self.format_val(a, f)?;
                write!(f, " is ")?;
                self.format_type(&Type::RcPointer(*class_id), f)
            }

            Instruction::AddrOf { out, a } => {
                write!(f, "{:>width$} ", "addrof", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := @")?;
                self.format_ref(a, f)
            }

            Instruction::Element {
                out,
                a,
                element,
                index,
            } => {
                write!(f, "{:>width$} ", "el", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := @(")?;
                self.format_ref(a, f)?;
                write!(f, " as array of ")?;
                self.format_type(element, f)?;
                write!(f, ")[")?;
                self.format_val(index, f)?;
                write!(f, "]")
            }

            Instruction::Field {
                out,
                a,
                of_ty,
                field,
            } => {
                write!(f, "{:>width$} ", "field", width = IX_WIDTH)?;

                self.format_ref(out, f)?;
                write!(f, " := @(")?;
                self.format_ref(a, f)?;
                write!(f, " as ")?;
                self.format_type(of_ty, f)?;
                write!(f, ").")?;
                self.format_field(of_ty, *field, f)
            }

            Instruction::VariantTag { out, a, of_ty } => {
                write!(f, "{:>width$} ", "vartag", width = IX_WIDTH)?;
                self.format_ref(out, f)?;
                write!(f, " := @(")?;
                self.format_ref(a, f)?;
                write!(f, " as ")?;
                self.format_type(of_ty, f)?;
                write!(f, ").tag")
            }

            Instruction::VariantData { out, a, of_ty, tag } => {
                write!(f, "{:>width$} ", "vardata", width = IX_WIDTH)?;
                self.format_ref(out, f)?;
                write!(f, " := @(")?;
                self.format_ref(a, f)?;
                write!(f, " as ")?;
                self.format_type(of_ty, f)?;
                write!(f, ").")?;
                self.format_variant_case(of_ty, *tag, f)
            }

            Instruction::Label(label) => {
                write!(f, "{:>width$} {}", "label", label, width = IX_WIDTH)
            }

            Instruction::Jump { dest } => write!(f, "{:>width$} {}", "jmp", dest, width = IX_WIDTH),

            Instruction::JumpIf { dest, test } => {
                write!(f, "{:>width$} {} if ", "jmpif", dest, width = IX_WIDTH)?;
                self.format_val(test, f)
            }

            Instruction::RcNew { out, type_id, immortal } => {
                write!(f, "{:>width$} ", "rcnew", width = IX_WIDTH)?;
                self.format_type(&Type::Struct(*type_id), f)?;
                write!(f, " at {}", out)?;
                if *immortal {
                    write!(f, " (immortal)")?;
                }
                Ok(())
            }

            Instruction::Release { at, weak } => {
                let ref_kind = if *weak { "weak" } else { "strong" };
                write!(f, "{:>width$} {} ({})", "release", at, ref_kind, width = IX_WIDTH)
            }

            Instruction::Retain { at, weak } => {
                let ref_kind = if *weak { "weak" } else { "strong" };
                write!(f, "{:>width$} {} ({})", "retain", at, ref_kind, width = IX_WIDTH)
            }

            Instruction::Raise { val } => {
                write!(f, "{:>width$} {}", "raise", val, width = IX_WIDTH)
            }

            Instruction::Cast { out, ty, a } => {
                write!(f, "{:>width$} {} := {} as ", "cast", out, a, width = IX_WIDTH)?;
                self.format_type(ty, f)
            }
        }
    }

    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result;
    fn format_method(
        &self,
        iface: InterfaceID,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result;
    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut dyn fmt::Write) -> fmt::Result;
    
    fn format_bin_op_instruction<W: fmt::Write>(&self,
        f: &mut W,
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

    fn format_prefix_op_instruction<W: fmt::Write>(&self,
        f: &mut W,
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

    fn format_name(&self, name: &NamePath, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", name.path.join("."))?;

        if let Some(name_type_args) = name.type_args.as_ref() {
            write!(f, "[")?;
            for (i, arg) in name_type_args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }

                self.format_type(arg, f)?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

pub struct RawInstructionFormatter;

impl IRFormatter for RawInstructionFormatter {
    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", ty)
    }

    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", val)
    }

    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", r)
    }

    fn format_field(&self, _of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result {
        write!(f, "{}", field)
    }

    fn format_method(
        &self,
        _iface_id: InterfaceID,
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

pub struct StatefulIndentedFormatter<'f, F: IRFormatter> {
    wrapped: &'f F,
    tabs: Cell<usize>,
    tab_width: usize,
}

impl<'f, F: IRFormatter> StatefulIndentedFormatter<'f, F> {
    pub fn new(wrapped: &'f F, tab_width: usize) -> Self {
        Self {
            wrapped,
            tabs: Cell::new(0),
            tab_width,
        }
    }
}

impl<'f, F: IRFormatter> IRFormatter for StatefulIndentedFormatter<'f, F> {
    fn format_instruction<W: fmt::Write>(
        &self,
        instruction: &Instruction,
        f: &mut W,
    ) -> fmt::Result {
        if let Instruction::LocalEnd = instruction {
            self.tabs.set(self.tabs.get() - 1);
        }

        let tabs = match instruction {
            Instruction::Label(..) => 0,
            _ => self.tabs.get(),
        };

        for _ in 0..tabs * self.tab_width {
            f.write_char(' ')?;
        }

        if let Instruction::LocalBegin = instruction {
            self.tabs.set(self.tabs.get() + 1);
        }

        self.wrapped.format_instruction(instruction, f)
    }

    fn format_type(&self, ty: &Type, f: &mut dyn fmt::Write) -> fmt::Result {
        self.wrapped.format_type(ty, f)
    }

    fn format_val(&self, val: &Value, f: &mut dyn fmt::Write) -> fmt::Result {
        self.wrapped.format_val(val, f)
    }

    fn format_ref(&self, r: &Ref, f: &mut dyn fmt::Write) -> fmt::Result {
        self.wrapped.format_ref(r, f)
    }

    fn format_field(&self, of_ty: &Type, field: FieldID, f: &mut dyn fmt::Write) -> fmt::Result {
        self.wrapped.format_field(of_ty, field, f)
    }

    fn format_method(
        &self,
        iface: InterfaceID,
        method: MethodID,
        f: &mut dyn fmt::Write,
    ) -> fmt::Result {
        self.wrapped.format_method(iface, method, f)
    }

    fn format_variant_case(&self, of_ty: &Type, tag: usize, f: &mut dyn fmt::Write) -> fmt::Result {
        self.wrapped.format_variant_case(of_ty, tag, f)
    }
}
