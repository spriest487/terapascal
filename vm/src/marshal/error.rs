use crate::DynValue;
use std::fmt;
use terapascal_ir as ir;
use terapascal_ir::IRFormatter;
use terapascal_ir::RawInstructionFormatter;
use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum MarshalError {
    InvalidData,

    UnsupportedType(ir::Type),
    UnsupportedValue(DynValue),

    VariantTagOutOfRange {
        variant_id: ir::TypeDefID,
        tag: DynValue,
    },
    FieldOutOfRange {
        struct_id: ir::TypeDefID,
        field: ir::FieldID,
    },
    InvalidTypeIndex {
        type_index: u64,
    },

    InvalidStructID {
        expected: ir::TypeDefID,
        actual: ir::TypeDefID,
    },

    InvalidRefCountValue(DynValue),

    ExternSymbolLoadFailed {
        lib: String,
        symbol: String,
        msg: String,
    },
    InvalidWrite {
        data_size: usize,
        dest_size: usize
    },
}

impl fmt::Display for MarshalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_pretty(f, &RawInstructionFormatter)
    }
}

impl MarshalError {
    pub fn fmt_pretty<Fmt>(&self, f: &mut fmt::Formatter, format: &Fmt) -> fmt::Result
    where
        Fmt: IRFormatter,
    {
        match self {
            MarshalError::InvalidData => write!(f, "invalid data"),
            MarshalError::InvalidStructID { expected, actual } => {
                write!(f, "expected struct ")?;
                format.format_type(&ir::Type::Struct(*expected), f)?;
                write!(f, ", got ")?;
                format.format_type(&ir::Type::Struct(*actual), f)?;
                Ok(())
            },
            MarshalError::InvalidTypeIndex { type_index: id }  => {
                write!(f, "unknown type ID in array: {}", id)
            }
            MarshalError::UnsupportedValue(val) => {
                write!(f, "unable to marshal value: {:?}", val)
            },
            MarshalError::UnsupportedType(ty) => {
                write!(f, "unable to marshal type: ")?;
                format.format_type(ty, f)?;
                Ok(())
            },
            MarshalError::ExternSymbolLoadFailed { lib, symbol, msg } => write!(
                f,
                "external symbol {}!{} failed to load ({})",
                lib, symbol, msg
            ),
            MarshalError::VariantTagOutOfRange { variant_id, tag } => write!(
                f,
                "tag {:?} for variant {} was out of range",
                tag, variant_id
            ),
            MarshalError::FieldOutOfRange { struct_id, field } => {
                let type_name = struct_id.to_pretty_string(format); 
                write!(f, "field {type_name}.{field} was out of range")
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

pub type MarshalResult<T> = Result<T, MarshalError>;
