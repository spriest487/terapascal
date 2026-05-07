use crate::DynValue;
use std::fmt;
use std::path::PathBuf;
use terapascal_ir as ir;
use thiserror::Error;
use crate::marshal::TypeIndex;

#[derive(Clone, Debug, Error)]
pub enum MarshalError<Ty = ir::Type> {
    InvalidData,

    MissingTypeDef(Ty),

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
        type_index: TypeIndex,
    },
    InvalidStructType(Ty),
    InvalidVariantType(Ty),
    InvalidObjectType(Ty),

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

    pub fn invalid_type_index(type_index: TypeIndex) -> Self {
        Self::InvalidTypeIndex { type_index }
    }

    pub fn map_types<F, ToTy>(self, f: F) -> MarshalError<ToTy>
    where
        F: Fn(Ty) -> ToTy,
    {
        match self {
            MarshalError::MissingTypeDef(ty) => {
                MarshalError::MissingTypeDef(f(ty))
            },
            MarshalError::UnsupportedType(ty) => {
                MarshalError::UnsupportedType(f(ty))
            },
            MarshalError::InvalidObjectType(ty) => {
                MarshalError::InvalidObjectType(f(ty))
            }
            MarshalError::VariantTagOutOfRange { variant_type, tag } => {
                MarshalError::VariantTagOutOfRange { variant_type: f(variant_type), tag }
            },
            MarshalError::FieldOutOfRange { struct_type, field } => {
                MarshalError::FieldOutOfRange { struct_type: f(struct_type), field }
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
            MarshalError::InvalidStructType(ty) => {
                MarshalError::InvalidStructType(f(ty))
            },
            MarshalError::InvalidVariantType(ty) => {
                MarshalError::InvalidVariantType(f(ty))
            }
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
            MarshalError::InvalidTypeIndex { type_index }  => {
                write!(f, "Invalid object pointer (unknown type index: {})", type_index)
            },
            MarshalError::InvalidStructType(ty)  => {
                write!(f, "Type {} is not a struct", ty)
            }
            MarshalError::InvalidVariantType(ty)  => {
                write!(f, "Type {} is not a variant", ty)
            }
            MarshalError::InvalidObjectType(ty)  => {
                write!(f, "Type {} is not an object type", ty)
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

            MarshalError::MissingTypeDef(ty) => {
                write!(f, "Missing definition for type {} in loaded metadata", ty)
            }
        }
    }
}

pub type MarshalResult<T, Ty = ir::Type> = Result<T, MarshalError<Ty>>;
