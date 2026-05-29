use crate::ir;
use crate::typ::TypeError;
use std::io;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum DigestError {
    #[error("{0}")]
    IOError(#[from] io::Error),

    #[error("{0}")]
    TypeError(#[from] TypeError),

    #[error("Invalid data")]
    InvalidData,

    #[error("Missing definition for function {0}")]
    MissingFuncDef(ir::FunctionID),

    #[error("Feature is not supported in imported packages: {0}")]
    UnsupportedFeature(String),
}

pub type DigestResult<T> = Result<T, DigestError>;

#[derive(Debug)]
pub enum DigestWarning {
    InvalidFunc(ir::FunctionID, DigestError),
}