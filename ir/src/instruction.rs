use crate::formatter::InstructionFormatter;
use crate::formatter::RawInstructionFormatter;
use crate::metadata::MethodID;
use crate::ty::FieldID;
use crate::ty::Type;
use crate::ty::VirtualTypeID;
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
    LocalBegin,
    LocalEnd,

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

    /// Stores the address of an array element from array at `a` into `out`
    Element {
        out: Ref,
        a: Ref,
        index: Value,
        element: Type,
    },

    /// stores a pointer to the tag of a variant at `a` into `out`
    VariantTag {
        out: Ref,
        a: Ref,
        of_ty: Type,
    },
    /// stores a pointer to the data for a variant case of index `tag` at `a` into `out`
    VariantData {
        out: Ref,
        a: Ref,
        tag: usize,
        of_ty: Type,
    },

    /// Stores the address of an object field from object of type `of_ty` at location `a` into `out`.
    /// `of_ty` must match the type of the value stored at `a` and must also be a structured type
    /// i.e. one that has fields (struct or RC-pointer to struct).
    Field {
        out: Ref,
        a: Ref,
        of_ty: Type,
        field: FieldID,
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
        class_id: VirtualTypeID,
    },

    Label(Label),
    Jump {
        dest: Label,
    },
    JumpIf {
        dest: Label,
        test: Value,
    },

    RcNew {
        out: Ref,
        type_id: TypeDefID,
        immortal: bool,
    },

    Release {
        at: Ref,
        weak: bool,
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
            | Instruction::LocalBegin
            | Instruction::LocalEnd
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
            | Instruction::Element { out, .. }
            | Instruction::VariantTag { out, .. }
            | Instruction::VariantData { out, .. }
            | Instruction::Field { out, .. }
            | Instruction::ClassIs { out, .. }
            | Instruction::RcNew { out, .. }
            | Instruction::Cast { out, .. } => *out == Ref::Discard,
        }
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
