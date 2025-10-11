use terapascal_common::span::Span;
use terapascal_common::span::Spanned;
use terapascal_common::DiagnosticLabel;
use terapascal_common::DiagnosticOutput;
use terapascal_common::Severity;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum PreprocessorError {
    #[error("Unrecognized directive `{directive}`")]
    IllegalDirective {
        directive: String,
        at: Span,
    },

    #[error("{err}")]
    IncludeError {
        filename: String,
        err: String,
        at: Span,
    },

    
    #[error("else or endif without matching ifdef or ifndef")]
    UnexpectedEndIf(Span),

    #[error("Unterminated conditional block")]
    UnterminatedCondition(Span),

    #[error("Unterminated comment")]
    UnterminatedComment(Span),
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
    fn severity(&self) -> Severity {
        Severity::Error
    }

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
