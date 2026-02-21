use std::ffi::OsString;
use std::io;
use terapascal_build::error::BuildError;
use terapascal_common::span::Span;
use terapascal_common::Backtrace;
use terapascal_common::DiagnosticMessage;
use terapascal_common::DiagnosticOutput;
use terapascal_common::Severity;
use terapascal_common::TracedError;
use terapascal_frontend::parse::ParseError;
use terapascal_frontend::pp::error::PreprocessorError;
use terapascal_frontend::typ::TypeError;
use terapascal_frontend::TokenizeError;
use terapascal_vm::result::ExecError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RunError {
    #[error(transparent)]
    BuildError(BuildError),

    #[error(transparent)]
    ExecError(ExecError),

    #[error("writing to file {} failed: {}", .0.file.display(), .1)]
    OutputFailed(Span, io::Error),

    #[error("internal compiler error")]
    InternalError(String),

    #[error("unknown output extension")]
    UnknownOutputExt(OsString),

    #[error("invalid arguments")]
    InvalidArguments(String),

    #[cfg(feature = "backend-c")]
    #[error("clang invocation failed")]
    ClangBuildFailed(io::Error),

    #[cfg(feature = "backend-cil")]
    #[error(transparent)]
    CilBuildError(#[from] terapascal_backend_cil::BuildError),
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
            RunError::UnknownOutputExt(ext) => DiagnosticMessage {
                severity,
                title: format!("extension {} is not supported on this platform", ext.display()),
                label: None,
                notes: Vec::new(),
            },
            RunError::InvalidArguments(err) => DiagnosticMessage {
                severity,
                title: err.to_string(),
                notes: Vec::new(),
                label: None,
            },
            #[cfg(feature = "backend-c")]
            RunError::ClangBuildFailed(err) => DiagnosticMessage {
                severity,
                title: format!("clang invocation failed: {err}"),
                notes: Vec::new(),
                label: None,
            },
            #[cfg(feature = "backend-cil")]
            RunError::CilBuildError(err) => DiagnosticMessage {
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
