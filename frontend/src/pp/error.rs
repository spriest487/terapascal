use terapascal_common::{
    span::{Span, Spanned},
    DiagnosticLabel, DiagnosticOutput,
};
use std::fmt;

#[derive(Debug, Clone)]
pub enum PreprocessorError {
    IllegalDirective {
        directive: String,
        at: Span,
    },
    IncludeError {
        filename: String,
        err: String,
        at: Span,
    },
    UnexpectedEndIf(Span),
    UnterminatedCondition(Span),
    UnterminatedComment(Span),
}

impl fmt::Display for PreprocessorError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PreprocessorError::IllegalDirective { directive, .. } => {
                write!(f, "unrecognized directive `{}`", directive)
            },

            PreprocessorError::UnexpectedEndIf(_) => {
                write!(f, "else or endif without matching ifdef or ifndef")
            },

            PreprocessorError::UnterminatedCondition(_) => {
                write!(f, "unterminated conditional block")
            },

            PreprocessorError::UnterminatedComment(_) => {
                write!(f, "unterminated comment")
            },

            PreprocessorError::IncludeError { err, .. } => {
                write!(f, "{}", err)
            },
        }
    }
}

impl Spanned for PreprocessorError {
    fn span(&self) -> &Span {
        match self {
            PreprocessorError::IllegalDirective { at, .. } => at,
            PreprocessorError::UnexpectedEndIf(at) => at,
            PreprocessorError::UnterminatedCondition(at) => at,
            PreprocessorError::UnterminatedComment(at) => at,
            PreprocessorError::IncludeError { at, .. } => at,
        }
    }
}

impl DiagnosticOutput for PreprocessorError {
    fn label(&self) -> Option<DiagnosticLabel> {
        Some(DiagnosticLabel {
            span: self.span().clone(),
            text: match self {
                PreprocessorError::IncludeError { filename, .. } => {
                    Some(format!("failed to read include file {}", filename))
                },
                _ => None,
            },
        })
    }
}
