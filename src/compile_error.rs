use std::fmt;
use std::io;
use terapascal_build::error::BuildError;
use terapascal_common::span::Span;
use terapascal_common::{Backtrace, Severity};
use terapascal_common::DiagnosticMessage;
use terapascal_common::DiagnosticOutput;
use terapascal_common::TracedError;
use terapascal_frontend::parse::ParseError;
use terapascal_frontend::pp::error::PreprocessorError;
use terapascal_frontend::typ::TypeError;
use terapascal_frontend::TokenizeError;
use terapascal_vm::result::ExecError;

#[derive(Debug)]
pub enum RunError {
    BuildError(BuildError),
    ExecError(ExecError),
    OutputFailed(Span, io::Error),
    InternalError(String),
    UnknownOutputFormat(String),
    ClangBuildFailed(io::Error),
    InvalidArguments(String),
}

impl From<TracedError<TokenizeError>> for RunError {
    fn from(err: TracedError<TokenizeError>) -> Self {
        Self::BuildError(BuildError::TokenizeError(err))
    }
}

impl From<TracedError<ParseError>> for RunError {
    fn from(err: TracedError<ParseError>) -> Self {
        Self::BuildError(BuildError::ParseError(err))
    }
}

impl From<TypeError> for RunError {
    fn from(err: TypeError) -> Self {
        Self::BuildError(BuildError::TypecheckError(err))
    }
}

impl From<PreprocessorError> for RunError {
    fn from(err: PreprocessorError) -> Self {
        Self::BuildError(BuildError::PreprocessorError(err))
    }
}

impl From<BuildError> for RunError {
    fn from(value: BuildError) -> Self {
        Self::BuildError(value)
    }
}

impl From<ExecError> for RunError {
    fn from(err: ExecError) -> Self {
        RunError::ExecError(err)
    }
}

impl From<bincode::error::EncodeError> for RunError {
    fn from(value: bincode::error::EncodeError) -> Self {
        RunError::InternalError(value.to_string())
    }
}

impl From<bincode::error::DecodeError> for RunError {
    fn from(value: bincode::error::DecodeError) -> Self {
        RunError::InternalError(value.to_string())
    }
}

impl From<io::Error> for RunError {
    fn from(err: io::Error) -> Self {
        RunError::InternalError(err.to_string())
    }
}

impl DiagnosticOutput for RunError {
    fn severity(&self) -> Severity {
        Severity::Error
    }
    
    fn main(&self) -> DiagnosticMessage {
        let severity = self.severity();
        match self {
            RunError::BuildError(err) => err.main(),
            RunError::OutputFailed(span, err) => DiagnosticMessage {
                severity,
                title: format!(
                    "Writing output file `{}` failed: {}",
                    span.file.display(),
                    err
                ),
                label: None,
                notes: Vec::new(),
            },
            RunError::ExecError(ExecError::Raised { msg, .. }) => DiagnosticMessage {
                severity,
                title: msg.clone(),
                label: None,
                notes: Vec::new(),
            },
            RunError::ExecError(err) => err.main(),
            RunError::InternalError(msg) => DiagnosticMessage {
                severity,
                title: msg.to_string(),
                label: None,
                notes: Vec::new(),
            },
            RunError::UnknownOutputFormat(ext) => DiagnosticMessage {
                severity,
                title: format!("extension {} is not supported on this platform", ext),
                label: None,
                notes: Vec::new(),
            },
            RunError::ClangBuildFailed(err) => DiagnosticMessage {
                severity,
                title: err.to_string(),
                notes: Vec::new(),
                label: None,
            },
            RunError::InvalidArguments(err) => DiagnosticMessage {
                severity,
                title: err.to_string(),
                notes: Vec::new(),
                label: None,
            }
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            RunError::BuildError(err) => err.see_also(),
            RunError::ExecError(err) => err.see_also(),
            _ => Vec::new(),
        }
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        match self {
            RunError::BuildError(err) => err.backtrace(),
            _ => None,
        }
    }
}

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RunError::BuildError(err) => write!(f, "{}", err),
            RunError::OutputFailed(span, err) => {
                write!(f, "writing to file {} failed: {}", span.file.display(), err)
            }
            RunError::ExecError(err) => write!(f, "{}", err),
            RunError::InternalError(..) => write!(f, "internal compiler error"),
            RunError::UnknownOutputFormat(..) => write!(f, "unknown output format"),
            RunError::ClangBuildFailed(..) => write!(f, "clang build failed"),
            RunError::InvalidArguments(..) => write!(f, "invalid arguments"),
        }
    }
}
