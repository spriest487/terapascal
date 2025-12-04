use crate::typ::ast::Literal;
use crate::IntConstant;
use crate::RealConstant;

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub enum Primitive {
    Boolean,
    UInt8,
    Int8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    NativeInt,
    NativeUInt,
    Real32,
    Real64,
    Pointer,
}

impl Primitive {
    pub const ALL: [Primitive; 14] = [
        Primitive::Boolean,
        Primitive::UInt8,
        Primitive::Int8,
        Primitive::Int16,
        Primitive::UInt16,
        Primitive::Int32,
        Primitive::UInt32,
        Primitive::Int64,
        Primitive::UInt64,
        Primitive::NativeInt,
        Primitive::NativeUInt,
        Primitive::Real32,
        Primitive::Real64,
        Primitive::Pointer,
    ];

    pub const fn is_numeric(&self) -> bool {
        match self {
            Primitive::Boolean => false,
            _ => true,
        }
    }
    
    // for types compatible with sets/range expressions
    pub fn integer_range(&self) -> Option<(IntConstant, IntConstant)> {
        match self {
            Primitive::Pointer | Primitive::Boolean | Primitive::Real32 | Primitive::Real64 => None,
            Primitive::UInt8 => {
                Some((IntConstant::from(u8::MIN), IntConstant::from(u8::MAX)))
            },
            Primitive::Int8 => {
                Some((IntConstant::from(i8::MIN), IntConstant::from(i8::MAX)))
            },
            Primitive::Int16 => {
                Some((IntConstant::from(i16::MIN), IntConstant::from(i16::MAX)))
            },
            Primitive::UInt16 => {
                Some((IntConstant::from(u16::MIN), IntConstant::from(u16::MAX)))
            },
            Primitive::Int32 => {
                Some((IntConstant::from(i32::MIN), IntConstant::from(i32::MAX)))
            },
            Primitive::UInt32 => {
                Some((IntConstant::from(u32::MIN), IntConstant::from(u32::MAX)))
            },
            Primitive::Int64 => {
                Some((IntConstant::from(i64::MIN), IntConstant::from(i64::MAX)))
            },
            Primitive::UInt64 => {
                Some((IntConstant::from(u64::MIN), IntConstant::from(u64::MAX)))
            },
            Primitive::NativeInt => {
                Some((IntConstant::from(isize::MIN), IntConstant::from(isize::MAX)))
            },
            Primitive::NativeUInt => {
                Some((IntConstant::from(usize::MIN), IntConstant::from(usize::MAX)))
            },
        }
    }

    pub const fn is_signed(&self) -> bool {
        match self {
            Primitive::Int8
            | Primitive::Int16
            | Primitive::Int32
            | Primitive::Int64
            | Primitive::NativeInt
            | Primitive::Real32
            | Primitive::Real64 => true,

            _ => false,
        }
    }

    pub const fn is_integer(&self) -> bool {
        match self {
            | Primitive::UInt8
            | Primitive::Int8
            | Primitive::Int16
            | Primitive::UInt16
            | Primitive::Int32
            | Primitive::UInt32
            | Primitive::Int64
            | Primitive::UInt64
            | Primitive::NativeInt
            | Primitive::NativeUInt
            | Primitive::Pointer => true,

            | _ => false,
        }
    }

    pub const fn is_pointer(&self) -> bool {
        match self {
            Primitive::Pointer => true,
            _ => false,
        }
    }

    pub const fn is_real(&self) -> bool {
        match self {
            | Primitive::Real32 => true,
            | Primitive::Real64 => true,
            | _ => false,
        }
    }

    pub const fn name(&self) -> &str {
        match self {
            Primitive::Boolean => "Boolean",
            Primitive::UInt8 => "UInt8",
            Primitive::Int8 => "Int8",
            Primitive::Int16 => "Int16",
            Primitive::UInt16 => "UInt16",
            Primitive::Int32 => "Int32",
            Primitive::UInt32 => "UInt32",
            Primitive::Int64 => "Int64",
            Primitive::UInt64 => "UInt64",
            Primitive::NativeInt => "NativeInt",
            Primitive::NativeUInt => "NativeUInt",
            Primitive::Real32 => "Real32",
            Primitive::Real64 => "Real64",
            Primitive::Pointer => "Pointer",
        }
    }

    pub fn default_val(&self) -> Literal {
        match self {
            | Primitive::Boolean => Literal::Boolean(false),
 
            Primitive::Real32
            | Primitive::Real64 => Literal::Real(RealConstant::from(0.0)),
            
            Primitive::Pointer => Literal::Nil,

            | Primitive::UInt8
            | Primitive::Int8
            | Primitive::Int16
            | Primitive::UInt16
            | Primitive::Int32
            | Primitive::UInt32
            | Primitive::Int64
            | Primitive::UInt64
            | Primitive::NativeInt
            | Primitive::NativeUInt => Literal::Integer(IntConstant::from(0)),
        }
    }
}
