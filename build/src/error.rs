use std::fmt;
use std::io;
use std::path::PathBuf;
use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::Backtrace;
use terapascal_common::DiagnosticLabel;
use terapascal_common::DiagnosticMessage;
use terapascal_common::DiagnosticOutput;
use terapascal_common::Severity;
use terapascal_common::TracedError;
use terapascal_frontend::ast::IdentPath;
use terapascal_frontend::ast::MainUnitKind;
use terapascal_frontend::import::ImportError;
use terapascal_frontend::parse::ParseError;
use terapascal_frontend::pp::error::PreprocessorError;
use terapascal_frontend::typ::TypeError;
use terapascal_frontend::TokenizeError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum BuildError {
    /// The build ran to completion but there were one or more errors in the build log
    CompletedWithErrors,

    TokenizeError(#[from] TracedError<TokenizeError>),
    ParseError(#[from] TracedError<ParseError>),
    TypecheckError(#[from] TypeError),
    ImportError(#[from] ImportError),
    PreprocessorError(#[from] PreprocessorError),
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
        unit_kind: MainUnitKind,
        existing_ident: Option<IdentPath>,
    },
    CircularDependency {
        unit_ident: IdentPath,
        used_unit: IdentPath,
        span: Span,
    },
    UnitNotLoaded {
        unit_name: IdentPath,
        used_in_unit: IdentPath,
    },
    UnitAlreadyImported {
        unit_ident: IdentPath,
        new_path: PathBuf,
    },
    IOError(#[from] io::Error),

    InternalError(String),
}

impl DiagnosticOutput for BuildError {
    fn severity(&self) -> Severity {
        Severity::Error
    }
    
    fn main(&self) -> DiagnosticMessage {
        match self {
            Self::CompletedWithErrors => {
                DiagnosticMessage::new(Severity::Error, "Build error")
            }
            Self::TokenizeError(err) => err.err.main(),
            Self::ParseError(err) => err.err.main(),
            Self::TypecheckError(err) => err.main(),
            Self::PreprocessorError(err) => err.main(),

            Self::ImportError(err) => {
                DiagnosticMessage::new(Severity::Error, err.to_string())
            },

            Self::FileNotFound(path, span) => {
                let title = format!("file not found: {}", path.display());
                let message = DiagnosticMessage::new(self.severity(), title);

                match span {
                    Some(span) => message.with_label(DiagnosticLabel::new(span.clone())),
                    None => message,
                }
            },

            Self::ReadSourceFileFailed { path, msg } => {
                let title = format!("failed to read source file {}", path.display());
                DiagnosticMessage::new(self.severity(), title)
                    .with_note(msg.clone())
            },

            Self::DuplicateUnit { unit_ident, new_path, existing_path } => {
                let title = format!(
                    "`{}` @ {} was already loaded from {}",
                    unit_ident,
                    new_path.display(),
                    existing_path.display()
                );

                DiagnosticMessage::new(self.severity(), title)
                    .with_label(DiagnosticLabel::new(unit_ident.span().clone()))
            }

            Self::UnexpectedMainUnit { unit_path, unit_kind, existing_ident } => {
                if let Some(ident) = existing_ident {
                    DiagnosticMessage::new(self.severity(), format!(
                        "encountered {} unit @ `{}` but main unit `{}` was already loaded",
                        unit_kind,
                        unit_path.display(),
                        ident
                    ))
                } else {
                    DiagnosticMessage::new(self.severity(), format!(
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

                DiagnosticMessage::new(self.severity(), title)
                    .with_label(DiagnosticLabel::new(span.clone())
                        .with_text("unit used here".to_string()))
            }

            Self::UnitNotLoaded { unit_name, used_in_unit } => {
                DiagnosticMessage::new(self.severity(), "reference to unit not loaded or imported by this project")
                    .with_label(DiagnosticLabel::new(unit_name.path_span()).with_text(format!("used unit `{unit_name}`")))
                    .with_note(format!("used by unit `{used_in_unit}`"))
            },

            Self::UnitAlreadyImported { unit_ident, new_path } => {
                let title = format!(
                    "unit `{}` used from `{}` is already provided by a referenced package",
                    unit_ident,
                    new_path.display(),
                );

                DiagnosticMessage::new(self.severity(), title)
                    .with_label(DiagnosticLabel::new(unit_ident.path_span()))
            }
            
            Self::IOError(err) => {
                DiagnosticMessage::new(Severity::Error, err.to_string())
            }

            Self::InternalError(msg) => {
                DiagnosticMessage::new(Severity::Error, "Internal compiler error")
                    .with_note(msg.clone())
            }
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
            Self::CompletedWithErrors => write!(f, "completed with errors"), 
            Self::TokenizeError(err) => write!(f, "{}", err.err),
            Self::ParseError(err) => write!(f, "{}", err.err),
            Self::TypecheckError(err) => write!(f, "{}", err),
            Self::PreprocessorError(err) => write!(f, "{}", err),
            Self::ImportError(err) => write!(f, "{}", err),
            Self::ReadSourceFileFailed { msg, .. } => write!(f, "{}", msg),
            Self::DuplicateUnit { .. } => write!(f, "unit was already loaded"),
            Self::FileNotFound(_, _) => write!(f, "file not found"),
            Self::CircularDependency { .. } => write!(f, "circular unit reference"),
            Self::UnexpectedMainUnit { .. } => write!(f, "unexpected main unit"),
            Self::UnitNotLoaded { .. } => write!(f, "unit not loaded"),
            Self::UnitAlreadyImported { .. } => write!(f, "unit already imported"),
            Self::IOError(err) => write!(f, "{}", err),
            Self::InternalError(err) => write!(f, "{}", err),
        }
    }
}

pub type BuildResult<T> = Result<T, BuildError>;
