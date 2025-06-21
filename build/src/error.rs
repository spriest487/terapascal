use std::path::PathBuf;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::{Backtrace, Severity};
use terapascal_common::DiagnosticLabel;
use terapascal_common::DiagnosticMessage;
use terapascal_common::DiagnosticOutput;
use terapascal_common::TracedError;
use terapascal_frontend::ast::IdentPath;
use terapascal_frontend::ast::UnitKind;
use terapascal_frontend::parse::ParseError;
use terapascal_frontend::pp::error::PreprocessorError;
use terapascal_frontend::typ::TypeError;
use terapascal_frontend::TokenizeError;

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
    fn main(&self, severity: Severity) -> DiagnosticMessage {
        match self {
            Self::TokenizeError(err) => err.err.main(severity),
            Self::ParseError(err) => err.err.main(severity),
            Self::TypecheckError(err) => err.main(severity),
            Self::PreprocessorError(err) => err.main(severity),

            Self::FileNotFound(path, span) => {
                let title = format!("file not found: {}", path.display());
                let message = DiagnosticMessage::new(severity, title);

                match span {
                    Some(span) => message.with_label(DiagnosticLabel::new(span.clone())),
                    None => message,
                }
            },

            Self::ReadSourceFileFailed { path, .. } => {
                let title = format!("failed to read source file {}", path.display());
                DiagnosticMessage::new(severity, title)
            },

            Self::DuplicateUnit { unit_ident, new_path, existing_path } => {
                let title = format!(
                    "`{}` @ {} was already loaded from {}",
                    unit_ident,
                    new_path.display(),
                    existing_path.display()
                );

                DiagnosticMessage::new(severity, title)
                    .with_label(DiagnosticLabel::new(unit_ident.span().clone()))
            }

            Self::UnexpectedMainUnit { unit_path, unit_kind, existing_ident } => {
                if let Some(ident) = existing_ident {
                    DiagnosticMessage::new(severity, format!(
                        "encountered {} unit @ `{}` but main unit `{}` was already loaded",
                        unit_kind,
                        unit_path.display(),
                        ident
                    ))
                } else {
                    DiagnosticMessage::new(severity, format!(
                        "encountered {} unit @ `{}` after other units were already loaded",
                        unit_kind,
                        unit_path.display()
                    ))
                }
            }

            Self::CircularDependency { unit_ident, used_unit, span } => {
                let title = format!(
                    "unit `{}` used from `{}` creates a circular reference",
                    used_unit, unit_ident
                );

                DiagnosticMessage::new(severity, title)
                    .with_label(DiagnosticLabel::new(span.clone())
                        .with_text("unit used here".to_string()))
            }

            Self::UnitNotLoaded { unit_name } => {
                let title = "used units must be referenced by the main unit or on the command line";
                DiagnosticMessage::new(severity, title)
                    .with_label(DiagnosticLabel::new(unit_name.path_span().clone()))
                    .with_note(format!("unit `{}` is not loaded", unit_name))
            },
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
