use crate::formatter::IRFormatter;
use crate::formatter::RawFormatter;
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
use std::ops::RangeBounds;
use std::{fmt, iter};
use terapascal_common::span::Span;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Comment(String),

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
        type_args: Vec<Type>,
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
        type_args: Vec<Type>,
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

    pub fn visit_refs<'a, F, Arg>(&'a mut self, arg: &mut Arg, f: &F)
    where
        F: Fn(&'a mut Ref, &mut Arg) + Sized,
    {
        match self {
            Instruction::Comment(..)
            | Instruction::LocalAlloc(..)
            | Instruction::Label(..)
            | Instruction::Jump { .. } => {},

            Instruction::Move { out, new_val } => {
                Self::visit_ref(out, f, arg);
                Self::visit_val(new_val, f, arg);
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
                Self::visit_ref(&mut bin_op.out, f, arg);
                Self::visit_val(&mut bin_op.a, f, arg);
                Self::visit_val(&mut bin_op.b, f, arg);
            },

            | Instruction::BitNot(unary_op) | Instruction::Not(unary_op) => {
                Self::visit_ref(&mut unary_op.out, f, arg);
                Self::visit_val(&mut unary_op.a, f, arg);
            },

            Instruction::Length { out, a, .. } => {
                Self::visit_ref(out, f, arg);
                Self::visit_ref(a, f, arg);
            }

            | Instruction::AddrOf { out, a }
            | Instruction::MakeRef { out, a } => {
                Self::visit_ref(out, f, arg);
                Self::visit_ref(a, f, arg);
            },

            Instruction::Cast { out, a, .. } | Instruction::ClassIs { out, a, .. } => {
                Self::visit_ref(out, f, arg);
                Self::visit_val(a, f, arg);
            },

            Instruction::Call {
                out,
                function,
                args,
                type_args: _,
            } => {
                if let Some(out_ref) = out {
                    Self::visit_ref(out_ref, f, arg);
                }

                Self::visit_val(function, f, arg);

                for call_arg in args {
                    Self::visit_val(call_arg, f, arg);
                }
            },

            Instruction::VirtualCall {
                out,
                self_arg,
                rest_args,
                ..
            } => {
                if let Some(out_ref) = out {
                    Self::visit_ref(out_ref, f, arg);
                }

                Self::visit_val(self_arg, f, arg);

                for call_arg in rest_args {
                    Self::visit_val(call_arg, f, arg);
                }
            },

            Instruction::JumpIf { test, .. } => {
                Self::visit_val(test, f, arg)
            },

            Instruction::NewObject { out, .. } => {
                Self::visit_ref(out, f, arg);
            },
            Instruction::NewArray { out, count, .. } => {
                Self::visit_ref(out, f, arg);
                Self::visit_val(count, f, arg);
            },
            Instruction::NewBox { out, .. } => {
                Self::visit_ref(out, f, arg);
            },

            Instruction::Release { at, released_out, .. }  => {
                Self::visit_ref(at, f, arg);
                Self::visit_ref(released_out, f, arg);
            },
            Instruction::Retain { at, .. } => {
                Self::visit_ref(at, f, arg);
            },

            Instruction::Raise { val } => {
                Self::visit_ref(val, f, arg);
            },
        }
    }

    fn visit_ref<'a, F, Arg>(r: &'a mut Ref, f: &F, arg: &mut Arg)
    where
        F: Fn(&'a mut Ref, &mut Arg),
    {
        match r {
            Ref::Deref(inner) => {
                Self::visit_val(inner.as_mut(), f, arg);
            }

            Ref::Field(field_ref) => {
                Self::visit_ref(&mut field_ref.instance, f, arg);
            }

            Ref::Element(element_ref) => {
                Self::visit_ref(&mut element_ref.instance, f, arg);
                Self::visit_val(&mut element_ref.index, f, arg);
            }

            Ref::VariantTag(tag_ref) => {
                Self::visit_ref(&mut tag_ref.instance, f, arg);
            }

            Ref::VariantData(data_ref) => {
                Self::visit_ref(&mut data_ref.instance, f, arg);
            }

            r => {
                f(r, arg);
            },
        }
    }

    fn visit_val<'a, F, Arg>(val: &'a mut Value, f: &F, arg: &mut Arg)
    where
        F: Fn(&'a mut Ref, &mut Arg),
    {
        let Value::Ref(val_ref) = val else {
            return;
        };

        Self::visit_ref(val_ref, f, arg);
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = String::new();
        RawFormatter
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

/// Contains a list of instructions and, if present, a list of source info spans where each entry
/// corresponds to the instruction at the same index in the instruction list.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstructionList {
    pub instructions: Vec<Instruction>,

    pub sources: Vec<Option<Span>>,
}

impl InstructionList {
    pub fn new() -> Self {
        InstructionList {
            instructions: Vec::new(),
            sources: Vec::new(),
        }
    }

    pub fn append(&mut self, other: &mut Self) {
        self.instructions.append(&mut other.instructions);

        self.sources.resize(self.instructions.len(), None);
        self.sources.append(&mut other.sources);
    }

    pub fn push(&mut self, instruction: Instruction, source: Option<Span>) {
        self.instructions.push(instruction);

        self.sources.resize(self.instructions.len() - 1, None);
        self.sources.push(source);
    }

    pub fn splice(&mut self,
        range: impl RangeBounds<usize> + Clone,
        instructions: impl IntoIterator<Item=Instruction>,
    ) {
        let instructions: Vec<_> = instructions.into_iter().collect();
        let sources = iter::repeat_n(None, instructions.len());

        self.instructions.splice(range.clone(), instructions);
        self.sources.splice(range, sources);
    }
    
    pub fn iter(&self) -> impl Iterator<Item=(&Instruction, Option<&Span>)> {
        let sources = self.sources
            .iter()
            .map(|span| span.as_ref())
            .chain(iter::repeat(None));

        self.instructions
            .iter()
            .zip(sources)
    }
}

pub trait AsInstruction {
    fn as_instruction(&self) -> &Instruction;
}

impl AsInstruction for Instruction {
    fn as_instruction(&self) -> &Instruction {
        self
    }
}

impl AsInstruction for &Instruction {
    fn as_instruction(&self) -> &Instruction {
        *self
    }
}