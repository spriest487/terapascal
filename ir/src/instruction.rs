use crate::formatter::IRFormatter;
use crate::formatter::RawInstructionFormatter;
use crate::metadata::MethodID;
use crate::ty::ObjectID;
use crate::ty::Type;
use crate::ty_decl::InterfaceID;
use crate::ty_decl::TypeDefID;
use crate::val::LocalID;
use crate::val::Ref;
use crate::val::Value;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use terapascal_common::span::Span;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Comment(String),
    DebugPush(Span),
    DebugPop,

    LocalAlloc(LocalID, Type),

    Move {
        out: Ref,
        new_val: Value,
    },

    Add(BinOpInstruction),
    Sub(BinOpInstruction),
    Mul(BinOpInstruction),
    IDiv(BinOpInstruction),
    FDiv(BinOpInstruction),
    Mod(BinOpInstruction),
    Shl(BinOpInstruction),
    Shr(BinOpInstruction),
    BitAnd(BinOpInstruction),
    BitOr(BinOpInstruction),
    BitXor(BinOpInstruction),
    BitNot(UnaryOpInstruction),

    Eq(BinOpInstruction),
    
    Gt(BinOpInstruction),
    Lt(BinOpInstruction),
    Lte(BinOpInstruction),
    Gte(BinOpInstruction),
    
    Not(UnaryOpInstruction),

    And(BinOpInstruction),
    Or(BinOpInstruction),

    /// Stores a pointer to `a` into `out`
    AddrOf {
        out: Ref,
        a: Ref,
    },
    // Stores a temporary reference to `a` into `out`
    MakeRef {
        out: Ref,
        a: Ref,
    },

    /// Get the length (number of elements) contained in the value at `a` of the given type,
    /// storing the result into `out`.
    /// If the type is not an array, the result is 1.
    Length {
        out: Ref,
        a: Ref,
        of_type: Type,
    },

    Call {
        out: Option<Ref>,
        function: Value,
        args: Vec<Value>,
    },
    VirtualCall {
        out: Option<Ref>,
        iface_id: InterfaceID,
        method: MethodID,
        self_arg: Value,
        rest_args: Vec<Value>,
    },
    ClassIs {
        out: Ref,
        a: Value,
        class_id: ObjectID,
    },

    Label(Label),
    Jump {
        dest: Label,
    },
    JumpIf {
        dest: Label,
        test: Value,
    },

    NewObject {
        out: Ref,
        type_id: TypeDefID,
        immortal: bool,
    },
    NewArray {
        out: Ref,
        element_type: Type,
        count: Value,
        immortal: bool,
    },
    NewBox {
        out: Ref,
        value_type: Type,
        immortal: bool,
    },

    Release {
        at: Ref,
        weak: bool,
        released_out: Ref,
    },
    Retain {
        at: Ref,
        weak: bool,
    },

    Raise {
        val: Ref,
    },

    Cast {
        out: Ref,
        ty: Type,
        a: Value,
    }
}

impl Instruction {
    // can this instruction be discarded? usually this means its parameters make it a noop,
    // e.g. mov or arithmetic into to a discard ref.
    //
    // intentional noop instructions like comments and debug info are not considered discardable.
    pub fn should_discard(&self) -> bool {
        match self {
            // never discard
            | Instruction::Comment(..)
            | Instruction::DebugPush(..)
            | Instruction::DebugPop
            | Instruction::Label(..)
            | Instruction::Jump { .. }
            | Instruction::JumpIf { .. }
            | Instruction::Raise { .. }
            | Instruction::VirtualCall { .. }
            | Instruction::Call { .. }
            | Instruction::LocalAlloc(..) => false,

            // instructions that mutate state
            // discard if they operate on a discard ref
            | Instruction::Release { at, .. }
            | Instruction::Retain { at, .. } => *at == Ref::Discard,

            // mov: discard if either the origin or destination refs are discards
            | Instruction::Move { out: Ref::Discard, .. }
            | Instruction::Move { new_val: Value::Ref(Ref::Discard), .. } => true,
            | Instruction::Move { .. } => false,

            // operator instructions
            // discard if they output into a discard ref
            | Instruction::Add(BinOpInstruction { out, .. })
            | Instruction::Sub(BinOpInstruction { out, .. })
            | Instruction::Mul(BinOpInstruction { out, .. })
            | Instruction::IDiv(BinOpInstruction { out, .. })
            | Instruction::FDiv(BinOpInstruction { out, .. })
            | Instruction::Mod(BinOpInstruction { out, .. })
            | Instruction::Shl(BinOpInstruction { out, .. })
            | Instruction::Shr(BinOpInstruction { out, .. })
            | Instruction::Eq(BinOpInstruction { out, .. })
            | Instruction::Gt(BinOpInstruction { out, .. })
            | Instruction::Gte(BinOpInstruction { out, .. })
            | Instruction::Lt(BinOpInstruction { out, .. })
            | Instruction::Lte(BinOpInstruction { out, .. })
            | Instruction::Not(UnaryOpInstruction { out, .. })
            | Instruction::And(BinOpInstruction { out, .. })
            | Instruction::Or(BinOpInstruction { out, .. })
            | Instruction::BitOr(BinOpInstruction { out, .. })
            | Instruction::BitAnd(BinOpInstruction { out, .. })
            | Instruction::BitXor(BinOpInstruction { out, .. })
            | Instruction::BitNot(UnaryOpInstruction { out, .. })
            | Instruction::AddrOf { out, .. }
            | Instruction::MakeRef { out, .. }
            | Instruction::Length { out, .. }
            | Instruction::ClassIs { out, .. }
            | Instruction::NewObject { out, .. }
            | Instruction::NewArray { out, .. }
            | Instruction::NewBox { out, .. }
            | Instruction::Cast { out, .. } => *out == Ref::Discard,
        }
    }

    pub fn visit_refs<F>(&mut self, f: &F)
    where
        F: Fn(&mut Ref) + Sized,
    {
        match self {
            Instruction::Comment(..)
            | Instruction::DebugPush(..)
            | Instruction::DebugPop
            | Instruction::LocalAlloc(..)
            | Instruction::Label(..)
            | Instruction::Jump { .. } => {},

            Instruction::Move { out, new_val } => {
                Self::visit_ref(out, f);
                Self::visit_val(new_val, f);
            },

            Instruction::Add(bin_op)
            | Instruction::Sub(bin_op)
            | Instruction::Mul(bin_op)
            | Instruction::IDiv(bin_op)
            | Instruction::FDiv(bin_op)
            | Instruction::Mod(bin_op)
            | Instruction::Shl(bin_op)
            | Instruction::Shr(bin_op)
            | Instruction::BitAnd(bin_op)
            | Instruction::BitOr(bin_op)
            | Instruction::BitXor(bin_op)
            | Instruction::Eq(bin_op)
            | Instruction::Gt(bin_op)
            | Instruction::Lt(bin_op)
            | Instruction::Lte(bin_op)
            | Instruction::Gte(bin_op)
            | Instruction::And(bin_op)
            | Instruction::Or(bin_op) => {
                Self::visit_ref(&mut bin_op.out, f);
                Self::visit_val(&mut bin_op.a, f);
                Self::visit_val(&mut bin_op.b, f);
            },

            | Instruction::BitNot(unary_op) | Instruction::Not(unary_op) => {
                Self::visit_ref(&mut unary_op.out, f);
                Self::visit_val(&mut unary_op.a, f);
            },

            Instruction::Length { out, a, .. } => {
                Self::visit_ref(out, f);
                Self::visit_ref(a, f);
            }

            | Instruction::AddrOf { out, a }
            | Instruction::MakeRef { out, a } => {
                Self::visit_ref(out, f);
                Self::visit_ref(a, f);
            },

            Instruction::Cast { out, a, .. } | Instruction::ClassIs { out, a, .. } => {
                Self::visit_ref(out, f);
                Self::visit_val(a, f);
            },

            Instruction::Call {
                out,
                function,
                args,
            } => {
                if let Some(out_ref) = out {
                    Self::visit_ref(out_ref, f);
                }

                Self::visit_val(function, f);

                for arg in args {
                    Self::visit_val(arg, f);
                }
            },

            Instruction::VirtualCall {
                out,
                self_arg,
                rest_args,
                ..
            } => {
                if let Some(out_ref) = out {
                    Self::visit_ref(out_ref, f);
                }

                Self::visit_val(self_arg, f);

                for arg in rest_args {
                    Self::visit_val(arg, f);
                }
            },

            Instruction::JumpIf { test, .. } => Self::visit_val(test, f),

            Instruction::NewObject { out, .. } => {
                Self::visit_ref(out, f);
            },
            Instruction::NewArray { out, count, .. } => {
                Self::visit_ref(out, f);
                Self::visit_val(count, f);
            },
            Instruction::NewBox { out, .. } => {
                Self::visit_ref(out, f);
            },

            Instruction::Release { at, released_out, .. }  => {
                Self::visit_ref(at, f);
                Self::visit_ref(released_out, f);
            },
            Instruction::Retain { at, .. } => {
                Self::visit_ref(at, f);
            },

            Instruction::Raise { val } => {
                Self::visit_ref(val, f);
            },
        }
    }

    fn visit_ref<F>(r: &mut Ref, f: F)
    where
        F: Fn(&mut Ref),
    {
        if let Ref::Deref(inner) = r {
            Self::visit_val(inner.as_mut(), f);
            return;
        }

        f(r);
    }

    fn visit_val<F>(val: &mut Value, f: F)
    where
        F: Fn(&mut Ref),
    {
        let Value::Ref(val_ref) = val else {
            return;
        };

        Self::visit_ref(val_ref, f);
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = String::new();
        RawInstructionFormatter
            .format_instruction(self, &mut buf)
            .map_err(|_| fmt::Error)?;

        f.write_str(&buf)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Label(pub usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BinOpInstruction {
    pub out: Ref,
    pub a: Value,
    pub b: Value,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnaryOpInstruction {
    pub out: Ref,
    pub a: Value,
}
