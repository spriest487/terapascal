use std::io;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RunError {
    #[error("Building library file failed")]
    BuildLibFailed,

    #[error("Building test binary failed")]
    BuildBinaryFailed,

    #[error("{0}")]
    IOError(#[from] io::Error),
}

pub type RunResult<T> = Result<T, RunError>;