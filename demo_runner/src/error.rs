use std::io;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RunError {
    #[error("Building library file failed")]
    BuildLibFailed,

    #[error("Building test binary failed")]
    BuildBinaryFailed,

    #[error("Invalid regular expression '{src}': {err}")]
    InvalidRegex {
        src: String,
        err: regex::Error,
    },

    #[error("{0}")]
    IOError(#[from] io::Error),

    #[error("Execution failed with code {0}")]
    ErrorCode(String),
}

pub type RunResult<T> = Result<T, RunError>;