use crate::ast::IdentPath;
use crate::ast::UnitKind;
use crate::parse::ParseError;
use crate::pp::error::PreprocessorError;
use crate::typ::TypeError;
use crate::TokenizeError;
use std::fmt;
use std::path::PathBuf;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::Backtrace;
use terapascal_common::DiagnosticLabel;
use terapascal_common::DiagnosticMessage;
use terapascal_common::DiagnosticOutput;
use terapascal_common::TracedError;

#[derive(Debug)]
pub enum BuildError {
    TokenizeError(TracedError<TokenizeError>),
    ParseError(TracedError<ParseError>),
    TypecheckError(TypeError),
    PreprocessorError(PreprocessorError),
    FileNotFound(PathBuf, Option<Span>),
    ReadSourceFileFailed {
        path: PathBuf,
        msg: String,
    },
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
}

impl From<TracedError<TokenizeError>> for BuildError {
    fn from(err: TracedError<TokenizeError>) -> Self {
        Self::TokenizeError(err)
    }
}

impl From<TracedError<ParseError>> for BuildError {
    fn from(err: TracedError<ParseError>) -> Self {
        Self::ParseError(err)
    }
}

impl From<TypeError> for BuildError {
    fn from(err: TypeError) -> Self {
        Self::TypecheckError(err)
    }
}

impl From<PreprocessorError> for BuildError {
    fn from(err: PreprocessorError) -> Self {
        Self::PreprocessorError(err)
    }
}

impl DiagnosticOutput for BuildError {
    fn main(&self) -> DiagnosticMessage {
        match self {
            Self::TokenizeError(err) => err.err.main(),
            Self::ParseError(err) => err.err.main(),
            Self::TypecheckError(err) => err.main(),
            Self::PreprocessorError(err) => err.main(),

            Self::FileNotFound(path, span) => DiagnosticMessage {
                title: format!("file not found: {}", path.display()),
                label: span.as_ref().map(|span| DiagnosticLabel {
                    text: None,
                    span: span.clone(),
                }),
                notes: Vec::new(),
            },
            Self::ReadSourceFileFailed { path, .. } => DiagnosticMessage {
                title: format!("failed to read source file {}", path.to_string_lossy()),
                label: None,
                notes: Vec::new(),
            },
            Self::DuplicateUnit {
                unit_ident,
                new_path,
                existing_path,
            } => DiagnosticMessage::new(format!(
                "`{}` @ {} was already loaded from {}",
                unit_ident,
                new_path.display(),
                existing_path.display()
            ))
            .with_label(DiagnosticLabel::new(unit_ident.span().clone())),
            Self::UnexpectedMainUnit {
                unit_path,
                unit_kind,
                existing_ident,
            } => {
                if let Some(ident) = existing_ident {
                    DiagnosticMessage::new(format!(
                        "encountered {} unit @ `{}` but main unit `{}` was already loaded",
                        unit_kind,
                        unit_path.display(),
                        ident
                    ))
                } else {
                    DiagnosticMessage::new(format!(
                        "encountered {} unit @ `{}` after other units were already loaded",
                        unit_kind,
                        unit_path.display()
                    ))
                }
            },
            Self::CircularDependency {
                unit_ident,
                used_unit,
                span,
            } => DiagnosticMessage {
                title: format!(
                    "unit `{}` used from `{}` creates a circular reference",
                    used_unit, unit_ident
                ),
                label: Some(DiagnosticLabel {
                    text: Some("unit used here".to_string()),
                    span: span.clone(),
                }),
                notes: Vec::new(),
            },
            Self::UnitNotLoaded { unit_name } => DiagnosticMessage::new(
                "used units must be referenced by the main unit or on the command line",
            )
            .with_label(DiagnosticLabel::new(unit_name.path_span().clone()))
            .with_note(format!("unit `{}` is not loaded", unit_name)),
        }
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        match self {
            Self::TokenizeError(err) => err.see_also(),
            Self::ParseError(err) => err.see_also(),
            Self::TypecheckError(err) => err.see_also(),
            Self::PreprocessorError(err) => err.see_also(),
            _ => Vec::new(),
        }
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        match self {
            Self::TokenizeError(err) => Some(&err.bt),
            Self::ParseError(err) => Some(&err.bt),
            _ => None,
        }
    }
}

impl fmt::Display for BuildError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::TokenizeError(err) => write!(f, "{}", err.err),
            Self::ParseError(err) => write!(f, "{}", err.err),
            Self::TypecheckError(err) => write!(f, "{}", err),
            Self::PreprocessorError(err) => write!(f, "{}", err),
            Self::ReadSourceFileFailed { msg, .. } => write!(f, "{}", msg),
            Self::DuplicateUnit { .. } => write!(f, "unit was already loaded"),
            Self::FileNotFound(_, _) => write!(f, "file not found"),
            Self::CircularDependency { .. } => write!(f, "circular unit reference"),
            Self::UnexpectedMainUnit { .. } => write!(f, "unexpected main unit"),
            Self::UnitNotLoaded { .. } => write!(f, "unit not loaded"),
        }
    }
}

pub type BuildResult<T> = Result<T, BuildError>;
