use crate::ir;
use crate::typ::Type;
use crate::typ::TypeError;
use std::fmt::Display;
use std::fmt::Formatter;
use std::io;
use terapascal_common::DiagnosticOutput;
use terapascal_common::Severity;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ImportError {
    #[error("{0}")]
    IOError(#[from] io::Error),

    #[error("{0}")]
    TypeError(#[from] TypeError),

    #[error("{0}")]
    InvalidData(String),

    #[error("Reference to missing string {0}")]
    MissingString(ir::StringID),

    #[error("Missing definition for function {0}")]
    MissingFuncDef(ir::FunctionID),

    #[error("Missing definition for method at index {0}")]
    MissingMethodDef(ir::MethodID),

    #[error("Missing definition for type {0}")]
    MissingTypeDef(String),

    #[error("Reference cannot be translated: {0}")]
    UnsupportedRef(String),

    #[error("Feature is not supported in imported packages: {0}")]
    UnsupportedFeature(String),
}

pub type ImportResult<T> = Result<T, ImportError>;

#[derive(Debug, Error)]
pub enum ImportWarning {
    InvalidType(String, Box<ImportError>),
    InvalidFunc(String, Box<ImportError>),
    InvalidMethodList(Type, Box<ImportError>),
    InvalidPath(String, Box<ImportError>),
}

impl Display for ImportWarning {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportWarning::InvalidType(name, err) => {
                write!(f, "Invalid type `{}`: {}", name, err)
            }

            ImportWarning::InvalidFunc(name, err) => {
                write!(f, "Invalid function `{}`: {}", name, err)
            }

            ImportWarning::InvalidMethodList(ty, err) => {
                write!(f, "Method list for type {ty} is invalid: {err}")
            }

            ImportWarning::InvalidPath(path, err) => {
                write!(f, "Failed to read path {path}: {err}")
            }
        }
    }
}

impl DiagnosticOutput for ImportWarning {
    fn severity(&self) -> Severity {
        Severity::Warning
    }
}