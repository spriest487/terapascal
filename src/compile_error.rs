use terapascal_common::span::{Span, Spanned};
use terapascal_common::Backtrace;
use terapascal_common::DiagnosticLabel;
use terapascal_common::DiagnosticMessage;
use terapascal_common::DiagnosticOutput;
use terapascal_common::TracedError;
use terapascal_frontend::ast::{IdentPath, UnitKind};
use terapascal_frontend::parse::ParseError;
use terapascal_frontend::pp::error::PreprocessorError;
use terapascal_frontend::typ::TypeError;
use terapascal_frontend::TokenizeError;
use std::fmt;
use std::io;
use std::path::PathBuf;
use terapascal_vm::result::ExecError;

#[derive(Debug)]
pub enum CompileError {
    TokenizeError(TracedError<TokenizeError>),
    ParseError(TracedError<ParseError>),
    TypecheckError(TypeError),
    PreprocessorError(PreprocessorError),
    ExecError(ExecError),

    FileNotFound(PathBuf, Option<Span>),
    ReadSourceFileFailed {
        path: PathBuf,
        msg: String,
    },
    OutputFailed(Span, io::Error),
    DuplicateUnit {
        unit_ident: IdentPath,
        new_path: PathBuf,
        existing_path: PathBuf,
    },
    UnexpectedMainUnit { 
        unit_path: PathBuf, 
        unit_kind: UnitKind,
        existing_ident: Option<IdentPath>,
    },
    CircularDependency {
        unit_ident: IdentPath,
        used_unit: IdentPath,
        span: Span,
    },
    UnitNotLoaded {
        unit_name: IdentPath,
    },
    InternalError(String),
    UnknownOutputFormat(String),
    ClangBuildFailed(io::Error),
    InvalidArguments(String),
}

impl From<TracedError<TokenizeError>> for CompileError {
    fn from(err: TracedError<TokenizeError>) -> Self {
        CompileError::TokenizeError(err)
    }
}

impl From<TracedError<ParseError>> for CompileError {
    fn from(err: TracedError<ParseError>) -> Self {
        CompileError::ParseError(err)
    }
}

impl From<TypeError> for CompileError {
    fn from(err: TypeError) -> Self {
        CompileError::TypecheckError(err)
    }
}

impl From<PreprocessorError> for CompileError {
    fn from(err: PreprocessorError) -> Self {
        CompileError::PreprocessorError(err)
    }
}

impl From<ExecError> for CompileError {
    fn from(err: ExecError) -> Self {
        CompileError::ExecError(err)
    }
}

impl From<bincode::error::EncodeError> for CompileError {
    fn from(value: bincode::error::EncodeError) -> Self {
        CompileError::InternalError(value.to_string())
    }
}

impl From<bincode::error::DecodeError> for CompileError {
    fn from(value: bincode::error::DecodeError) -> Self {
        CompileError::InternalError(value.to_string())
    }
}

impl From<io::Error> for CompileError {
    fn from(err: io::Error) -> Self {
        CompileError::InternalError(err.to_string())
    }
}

impl DiagnosticOutput for CompileError {
    fn main(&self) -> DiagnosticMessage {
        match self {
            CompileError::TokenizeError(err) => err.err.main(),
            CompileError::ParseError(err) => err.err.main(),
            CompileError::TypecheckError(err) => err.main(),
            CompileError::PreprocessorError(err) => err.main(),
            CompileError::OutputFailed(span, err) => DiagnosticMessage {
                title: format!(
                    "Writing output file `{}` failed: {}",
                    span.file.display(),
                    err
                ),
                label: None,
                notes: Vec::new(),
            },
            CompileError::ExecError(ExecError::Raised { msg, .. }) => DiagnosticMessage {
                title: msg.clone(),
                label: None,
                notes: Vec::new(),
            },
            CompileError::ExecError(err) => err.main(),
            CompileError::FileNotFound(path, span) => DiagnosticMessage {
                title: format!("file not found: {}", path.display()),
                label: span.as_ref().map(|span| DiagnosticLabel {
                    text: None,
                    span: span.clone(),
                }),
                notes: Vec::new(),
            },
            CompileError::ReadSourceFileFailed { path, .. } => DiagnosticMessage {
                title: format!("failed to read source file {}", path.to_string_lossy()),
                label: None,
                notes: Vec::new(),
            },
            CompileError::DuplicateUnit { unit_ident, new_path, existing_path } => {
                DiagnosticMessage::new(format!("`{}` @ {} was already loaded from {}", unit_ident, new_path.display(), existing_path.display()))
                    .with_label(DiagnosticLabel::new(unit_ident.span().clone()))
            },
            CompileError::UnexpectedMainUnit { unit_path, unit_kind, existing_ident } => {
                if let Some(ident) = existing_ident {
                    DiagnosticMessage::new(format!("encountered {} unit @ `{}` but main unit `{}` was already loaded", unit_kind, unit_path.display(), ident))
                } else {
                    DiagnosticMessage::new(format!("encountered {} unit @ `{}` after other units were already loaded", unit_kind, unit_path.display()))
                }
            }
            CompileError::CircularDependency { unit_ident, used_unit, span } => DiagnosticMessage {
                title: format!("unit `{}` used from `{}` creates a circular reference", used_unit, unit_ident),
                label: Some(DiagnosticLabel {
                    text: Some("unit used here".to_string()),
                    span: span.clone(),
                }),
                notes: Vec::new(),
            },
            CompileError::UnitNotLoaded { unit_name } => {
                DiagnosticMessage::new("used units must be referenced by the main unit or on the command line")
                    .with_label(DiagnosticLabel::new(unit_name.path_span().clone()))
                    .with_note(format!("unit `{}` is not loaded", unit_name))
            },
            CompileError::InternalError(msg) => DiagnosticMessage {
                title: msg.to_string(),
                label: None,
                notes: Vec::new(),
            },
            CompileError::UnknownOutputFormat(ext) => DiagnosticMessage {
                title: format!("extension {} is not supported on this platform", ext),
                label: None,
                notes: Vec::new(),
            },
            CompileError::ClangBuildFailed(err) => DiagnosticMessage {
                title: err.to_string(),
                notes: Vec::new(),
                label: None,
            },
            CompileError::InvalidArguments(err) => DiagnosticMessage {
                title: err.to_string(),
                notes: Vec::new(),
                label: None,
            }
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            CompileError::TokenizeError(err) => err.see_also(),
            CompileError::ParseError(err) => err.see_also(),
            CompileError::TypecheckError(err) => err.see_also(),
            CompileError::PreprocessorError(err) => err.see_also(),
            CompileError::ExecError(err) => err.see_also(),
            _ => Vec::new(),
        }
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        match self {
            CompileError::TokenizeError(err) => Some(&err.bt),
            CompileError::ParseError(err) => Some(&err.bt),
            _ => None,
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompileError::TokenizeError(err) => write!(f, "{}", err.err),
            CompileError::ParseError(err) => write!(f, "{}", err.err),
            CompileError::TypecheckError(err) => write!(f, "{}", err),
            CompileError::PreprocessorError(err) => write!(f, "{}", err),
            CompileError::ReadSourceFileFailed { msg, .. } => write!(f, "{}", msg),
            CompileError::OutputFailed(span, err) => {
                write!(f, "writing to file {} failed: {}", span.file.display(), err)
            }
            CompileError::ExecError(err) => write!(f, "{}", err),
            CompileError::DuplicateUnit { .. } => write!(f, "unit was already loaded"),
            CompileError::FileNotFound(_, _) => write!(f, "file not found"),
            CompileError::CircularDependency { .. } => write!(f, "circular unit reference"),
            CompileError::UnexpectedMainUnit { .. } => write!(f, "unexpected main unit"),
            CompileError::UnitNotLoaded { .. } => write!(f, "unit not loaded"),
            CompileError::InternalError(..) => write!(f, "internal compiler error"),
            CompileError::UnknownOutputFormat(..) => write!(f, "unknown output format"),
            CompileError::ClangBuildFailed(..) => write!(f, "clang build failed"),
            CompileError::InvalidArguments(..) => write!(f, "invalid arguments"),
        }
    }
}
