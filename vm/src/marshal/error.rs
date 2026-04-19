use crate::DynValue;
use std::fmt;
use std::path::PathBuf;
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
        path: PathBuf,
        msg: String,
        cause: Option<String>,
    },
    InvalidWrite {
        data_size: usize,
        dest_size: usize
    },
}

impl<Ty> MarshalError<Ty> {
    pub fn unsupported_type(ty: Ty) -> Self {
        Self::UnsupportedType(ty)
    }

    pub fn map_types<F, ToTy>(self, f: F) -> MarshalError<ToTy>
    where
        F: Fn(Ty) -> ToTy,
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
            MarshalError::ExternSymbolLoadFailed { lib, symbol, path: filename, msg, cause } => {
                MarshalError::ExternSymbolLoadFailed { lib, symbol, path: filename, msg, cause }
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
            MarshalError::InvalidData => {
                write!(f, "Invalid data")
            },
            MarshalError::InvalidStructID { expected, actual } => {
                write!(f, "Expected struct {expected}, got {actual}")?;
                Ok(())
            },
            MarshalError::InvalidTypeIndex { type_index: id }  => {
                write!(f, "Illegal object pointer (unknown type ID: {})", id)
            }
            MarshalError::UnsupportedValue(val) => {
                write!(f, "Unable to marshal value: {:?}", val)
            },
            MarshalError::UnsupportedType(ty) => {
                write!(f, "Unable to marshal type: {ty}")
            },
            MarshalError::ExternSymbolLoadFailed { lib, symbol, path, msg, cause } => {
                write!(f, "External symbol {lib}::{symbol} ({}) failed to load: {msg}", path.display())?;
                if let Some(cause) = cause {
                    write!(f, " ({cause})")?;
                }
                Ok(())
            },
            MarshalError::VariantTagOutOfRange { variant_type, tag } => {
                write!(f, "Tag {tag:?} for variant {variant_type} was out of range")
            },
            MarshalError::FieldOutOfRange { struct_type, field } => {
                write!(f, "Field {struct_type}.{field} was out of range")
            },
            MarshalError::InvalidRefCountValue(val) => {
                write!(f, "Value is not a valid ref count: {:?}", val)
            },
            MarshalError::InvalidWrite { dest_size, data_size } => {
                write!(f, "Writing {} bytes to buffer of size {}", data_size, dest_size)
            }
        }
    }
}

pub type MarshalResult<T, Ty = ir::Type> = Result<T, MarshalError<Ty>>;
