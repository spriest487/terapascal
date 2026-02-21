use crate::metadata::StringID;
use crate::ty::Type;
use crate::ty_decl::TagLocation;
use crate::FunctionID;
use crate::IRFormatter;
use crate::StaticClosureID;
use crate::VariableID;
use bigdecimal::BigDecimal;
use bigdecimal::FromPrimitive;
use bigdecimal::ToPrimitive;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Ref {
    /// write-only ref that doesn't result in mov instructions when written to
    Discard,
    Result,
    Arg(ArgID),
    Local(LocalID),
    Global(GlobalRef),
    Deref(Box<Value>),
}

impl Ref {
    pub fn to_deref(self) -> Self {
        Ref::Deref(Box::new(Value::Ref(self)))
    }

    pub fn value(self) -> Value {
        Value::Ref(self)
    }

    pub fn to_pretty_string(&self, formatter: &impl IRFormatter) -> String {
        let mut string = String::new();
        _ = formatter.format_ref(self, &mut string);

        string
    }
    
    pub fn is_discard(&self) -> bool {
        matches!(self, Ref::Discard)
    }
}

impl From<GlobalRef> for Ref {
    fn from(global_ref: GlobalRef) -> Self {
        Ref::Global(global_ref)
    }
}

impl From<LocalID> for Ref {
    fn from(id: LocalID) -> Self {
        Ref::Local(id)
    }
}

impl From<ArgID> for Ref {
    fn from(id: ArgID) -> Self {
        Ref::Arg(id)
    }
}

impl From<FunctionID> for Ref {
    fn from(id: FunctionID) -> Self {
        Ref::Global(GlobalRef::Function(id))
    }
}

impl From<VariableID> for Ref {
    fn from(id: VariableID) -> Self {
        Ref::Global(GlobalRef::Variable(id))
    }
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ref::Discard => write!(f, "_"),
            Ref::Result => write!(f, "%Result"),
            Ref::Arg(id) => write!(f, "{id}"),
            Ref::Local(id) => write!(f, "{}", id),
            Ref::Global(name) => write!(f, "{}", name),
            Ref::Deref(at) => write!(f, "{}^", at),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Ref(Ref),
    LiteralNull,
    LiteralBool(bool),
    LiteralU8(u8),
    LiteralI8(i8),
    LiteralI16(i16),
    LiteralU16(u16),
    LiteralI32(i32),
    LiteralU32(u32),
    LiteralI64(i64),
    LiteralU64(u64),
    LiteralF32(f32),
    LiteralF64(f64),
    LiteralISize(isize),
    LiteralUSize(usize),
    SizeOf(Type),
    Default(Type),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Ref(r) => write!(f, "{}", r),
            Value::LiteralU8(i) => write!(f, "{}u8", i),
            Value::LiteralI8(i) => write!(f, "{}i8", i),
            Value::LiteralI16(i) => write!(f, "{}i16", i),
            Value::LiteralU16(i) => write!(f, "{}u16", i),
            Value::LiteralI32(i) => write!(f, "{}i32", i),
            Value::LiteralU32(i) => write!(f, "{}u32", i),
            Value::LiteralI64(i) => write!(f, "{}i64", i),
            Value::LiteralU64(i) => write!(f, "{}u64", i),
            Value::LiteralISize(i) => write!(f, "{}isize", i),
            Value::LiteralUSize(i) => write!(f, "{}usize", i),
            Value::LiteralBool(b) => write!(f, "{}", b),
            Value::LiteralF32(x) => write!(f, "{:.6}", x),
            Value::LiteralF64(x) => write!(f, "{:.15}", x),
            Value::LiteralNull => write!(f, "NULL"),
            Value::SizeOf(ty) => write!(f, "sizeof({ty})"),
            Value::Default(ty) => write!(f, "default({ty})"),
        }
    }
}

impl From<Ref> for Value {
    fn from(r: Ref) -> Self {
        Value::Ref(r)
    }
}

impl From<LocalID> for Value {
    fn from(local_id: LocalID) -> Self {
        Self::from(Ref::Local(local_id))
    }
}

impl From<ArgID> for Value {
    fn from(arg_id: ArgID) -> Self {
        Self::from(Ref::Arg(arg_id))
    }
}

impl From<GlobalRef> for Value {
    fn from(global_ref: GlobalRef) -> Self {
        Self::from(Ref::Global(global_ref))
    }
}

impl From<FunctionID> for Value {
    fn from(id: FunctionID) -> Self {
        Self::from(Ref::Global(GlobalRef::Function(id)))
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::LiteralBool(value)
    }
}

impl Value {
    pub const I32_0: Self = Self::LiteralI32(0);
    pub const I32_1: Self = Self::LiteralI32(1);
    
    pub fn deref(self) -> Ref {
        Ref::Deref(Box::new(self))
    }

    pub fn to_literal_bool(&self) -> Option<bool> {
        match self {
            Value::LiteralBool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn to_literal_val(&self) -> Option<BigDecimal> {
        match self {
            Value::LiteralU8(x) => Some(BigDecimal::from(*x)),
            Value::LiteralI8(x) => Some(BigDecimal::from(*x)),

            Value::LiteralI16(x) => Some(BigDecimal::from(*x)),
            Value::LiteralU16(x) => Some(BigDecimal::from(*x)),

            Value::LiteralI32(x) => Some(BigDecimal::from(*x)),
            Value::LiteralU32(x) => Some(BigDecimal::from(*x)),

            Value::LiteralI64(x) => Some(BigDecimal::from(*x)),
            Value::LiteralU64(x) => Some(BigDecimal::from(*x)),

            Value::LiteralISize(x) => Some(BigDecimal::from(*x as i64)),
            Value::LiteralUSize(x) => Some(BigDecimal::from(*x as u64)),

            Value::LiteralF32(x) => Some(
                BigDecimal::from_f32(*x).expect("NaN/infinite constant values not supported yet"),
            ),
            Value::LiteralF64(x) => Some(
                BigDecimal::from_f64(*x).expect("NaN/infinite constant values not supported yet"),
            ),
            _ => None,
        }
    }

    pub fn from_literal_val(val: BigDecimal, as_type: &Type) -> Option<Self> {
        match as_type {
            Type::U8 => Some(Value::LiteralU8(val.to_u8()?)),
            Type::I8 => Some(Value::LiteralI8(val.to_i8()?)),

            Type::U16 => Some(Value::LiteralU16(val.to_u16()?)),
            Type::I16 => Some(Value::LiteralI16(val.to_i16()?)),

            Type::U32 => Some(Value::LiteralU32(val.to_u32()?)),
            Type::I32 => Some(Value::LiteralI32(val.to_i32()?)),

            Type::U64 => Some(Value::LiteralU64(val.to_u64()?)),
            Type::I64 => Some(Value::LiteralI64(val.to_i64()?)),

            Type::USize => Some(Value::LiteralUSize(val.to_usize()?)),
            Type::ISize => Some(Value::LiteralISize(val.to_isize()?)),

            Type::F32 => Some(Value::LiteralF32(val.to_f32()?)),

            _ => None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub enum GlobalRef {
    Function(FunctionID),
    StringLiteral(StringID),
    StaticClosure(StaticClosureID),
    StaticTypeInfo(Rc<Type>),
    StaticFuncInfo(FunctionID),
    Variable(VariableID),
    StaticTagArray(TagLocation),
}

impl fmt::Display for GlobalRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GlobalRef::Function(func_id) => write!(f, "{}", func_id),
            GlobalRef::StringLiteral(id) => write!(f, "{}", id),
            GlobalRef::StaticClosure(id) => write!(f, "{}", id),
            GlobalRef::Variable(id) => write!(f, "{}", id),
            GlobalRef::StaticTypeInfo(ty) => write!(f, "typeinfo({})", ty),
            GlobalRef::StaticFuncInfo(id) => write!(f, "funcinfo({})", id),
            GlobalRef::StaticTagArray(id) => write!(f, "{}", id),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub struct LocalID(pub usize);

impl fmt::Display for LocalID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl LocalID {
    pub fn to_ref(self) -> Ref {
        Ref::Local(self)
    }

    pub fn to_deref(self) -> Ref {
        self.to_ref().to_deref()
    }
    
    pub fn value(self) -> Value {
        self.to_ref().value()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub struct ArgID(pub usize);

impl fmt::Display for ArgID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%Arg{}", self.0)
    }
}

impl ArgID {
    pub fn to_ref(self) -> Ref {
        Ref::Arg(self)
    }

    pub fn to_deref(self) -> Ref {
        self.to_ref().to_deref()
    }

    pub fn value(self) -> Value {
        self.to_ref().value()
    }
}
