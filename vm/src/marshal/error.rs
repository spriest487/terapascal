use crate::DynValue;
use std::fmt;
use terapascal_ir as ir;
use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum MarshalError<Ty = ir::Type> {
    InvalidData,

    UnsupportedType(Ty),
    UnsupportedValue(DynValue),

    VariantTagOutOfRange {
        variant_type: Ty,
        tag: DynValue,
    },
    FieldOutOfRange {
        struct_type: Ty,
        field: ir::FieldID,
    },
    InvalidTypeIndex {
        type_index: u64,
    },

    InvalidStructID {
        expected: Ty,
        actual: Ty,
    },

    InvalidRefCountValue(DynValue),

    ExternSymbolLoadFailed {
        lib: String,
        symbol: String,
        msg: String,
        cause: Option<String>,
    },
    InvalidWrite {
        data_size: usize,
        dest_size: usize
    },
}

impl<Ty: fmt::Display> MarshalError<Ty> {
    pub fn map_types<F, ToTy>(self, f: F) -> MarshalError<ToTy>
    where
        F: Fn(Ty) -> ToTy,
        ToTy: fmt::Display,
    {
        match self {
            MarshalError::UnsupportedType(ty) => {
                MarshalError::UnsupportedType(f(ty))
            },
            MarshalError::VariantTagOutOfRange { variant_type, tag } => {
                MarshalError::VariantTagOutOfRange { variant_type: f(variant_type), tag }
            },
            MarshalError::FieldOutOfRange { struct_type, field } => {
                MarshalError::FieldOutOfRange { struct_type: f(struct_type), field }
            },
            MarshalError::InvalidStructID { expected, actual } => {
                MarshalError::InvalidStructID { expected: f(expected), actual: f(actual) }
            },
            MarshalError::InvalidData => {
                MarshalError::InvalidData
            },
            MarshalError::UnsupportedValue(val) => {
                MarshalError::UnsupportedValue(val)
            },
            MarshalError::InvalidTypeIndex { type_index } => {
                MarshalError::InvalidTypeIndex { type_index }
            },
            MarshalError::InvalidRefCountValue(val) => {
                MarshalError::InvalidRefCountValue(val)
            },
            MarshalError::ExternSymbolLoadFailed { lib, symbol, msg, cause } => {
                MarshalError::ExternSymbolLoadFailed { lib, symbol, msg, cause }
            },
            MarshalError::InvalidWrite { dest_size, data_size } => {
                MarshalError::InvalidWrite { dest_size, data_size }
            },
        }
    }
}

impl<T: fmt::Display> fmt::Display for MarshalError<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MarshalError::InvalidData => write!(f, "invalid data"),
            MarshalError::InvalidStructID { expected, actual } => {
                write!(f, "expected struct {expected}, got {actual}")?;
                Ok(())
            },
            MarshalError::InvalidTypeIndex { type_index: id }  => {
                write!(f, "unknown type ID: {}", id)
            }
            MarshalError::UnsupportedValue(val) => {
                write!(f, "unable to marshal value: {:?}", val)
            },
            MarshalError::UnsupportedType(ty) => {
                write!(f, "unable to marshal type: {ty}")
            },
            MarshalError::ExternSymbolLoadFailed { lib, symbol, msg, cause } => {
                write!(f, "external symbol {lib}!{symbol} failed to load: {msg}")?;
                if let Some(cause) = cause {
                    write!(f, " ({cause})")?;
                }
                Ok(())
            },
            MarshalError::VariantTagOutOfRange { variant_type, tag } => {
                write!(f, "tag {tag:?} for variant {variant_type} was out of range")
            },
            MarshalError::FieldOutOfRange { struct_type, field } => {
                write!(f, "field {struct_type}.{field} was out of range")
            },
            MarshalError::InvalidRefCountValue(val) => {
                write!(f, "value is not a valid ref count: {:?}", val)
            },
            MarshalError::InvalidWrite { dest_size, data_size } => {
                write!(f, "writing {} bytes to buffer of size {}", data_size, dest_size)
            }
        }
    }
}

pub type MarshalResult<T, Ty = ir::Type> = Result<T, MarshalError<Ty>>;
