use crate::heap::NativeHeapError;
use crate::ir;
use crate::marshal::MarshalError;
use crate::stack::StackError;
use crate::stack::StackTrace;
use crate::Pointer;
use std::fmt;
use std::path::PathBuf;
use terapascal_common::DiagnosticLabel;
use terapascal_common::DiagnosticOutput;
use terapascal_common::Severity;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ExecError<Ty = ir::Type> {
    Raised {
        msg: String,
    },
    MarshalError(MarshalError<Ty>),
    StackError(#[from] StackError),
    ExternSymbolLoadFailed {
        lib: String,
        symbol: String,
        path: PathBuf,
        msg: String,
        cause: Option<String>,
    },
    IllegalDereference {
        ptr: Pointer,
    },
    IllegalInstruction(ir::Instruction),
    InternalError {
        msg: String,
    },
    NativeHeapError(#[from] NativeHeapError<Ty>),
    ZeroLengthAllocation,
    WithStackTrace {
        err: Box<ExecError<Ty>>,
        stack_trace: StackTrace,
    }
}

impl<Ty> ExecError<Ty> {
    pub fn with_stack_trace(err: impl Into<Self>, stack_trace: StackTrace) -> Self {
        Self::WithStackTrace {
            err: Box::new(err.into()),
            stack_trace,
        }
    }
}

impl<Ty: fmt::Display> ExecError<Ty> {
    pub fn illegal_state(msg: impl Into<String>) -> Self {
        Self::InternalError {
            msg: msg.into(),
        }
    }

    pub fn map_types<F, ToTy>(self, f: F) -> ExecError<ToTy>
    where
        F: Fn(Ty) -> ToTy,
        ToTy: fmt::Display,
    {
        match self {
            ExecError::Raised { msg } => {
                ExecError::Raised { msg }
            },
            ExecError::MarshalError(err) => {
                ExecError::MarshalError(err.map_types(f))
            },
            ExecError::StackError(err) => {
                ExecError::StackError(err)
            },
            ExecError::ExternSymbolLoadFailed { msg, lib, path, symbol, cause } => {
                ExecError::ExternSymbolLoadFailed { msg, lib, path, symbol, cause }
            },
            ExecError::IllegalDereference { ptr } => {
                ExecError::IllegalDereference { ptr }
            },
            ExecError::IllegalInstruction(illegal) => {
                ExecError::IllegalInstruction(illegal)
            },
            ExecError::InternalError { msg } => {
                ExecError::InternalError { msg }
            },
            ExecError::NativeHeapError(err) => {
                ExecError::NativeHeapError(err.map_types(f))
            },
            ExecError::ZeroLengthAllocation => {
                ExecError::ZeroLengthAllocation
            },
            ExecError::WithStackTrace { err, stack_trace } => {
                ExecError::WithStackTrace { err: Box::new((*err).map_types(f)), stack_trace }
            },
        }
    }
}

impl<Ty: fmt::Display> fmt::Display for ExecError<Ty> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.title())
    }
}

impl<Ty: fmt::Display> DiagnosticOutput for ExecError<Ty> {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn title(&self) -> String {
        match self {
            ExecError::Raised { msg } => msg.clone(),
            ExecError::MarshalError(..) => "Memory error".to_string(),
            ExecError::StackError(..) => "Stack error".to_string(),
            ExecError::ExternSymbolLoadFailed { .. } => "Symbol load failed".to_string(),
            ExecError::IllegalDereference { .. } => "Illegal dereference".to_string(),
            ExecError::InternalError { .. } => "Internal VM error".to_string(),
            ExecError::NativeHeapError(err) => err.to_string(),
            ExecError::ZeroLengthAllocation => "Dynamic allocation with length 0".to_string(),
            ExecError::IllegalInstruction(..) => "Illegal instruction".to_string(),
            ExecError::WithStackTrace { err, .. } => err.title(),
        }
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            ExecError::WithStackTrace { stack_trace, .. } => {
                let span = stack_trace.top_span()?;

                Some(DiagnosticLabel {
                    text: None,
                    span,
                })
            },

            _ => None,
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            ExecError::Raised { .. } => Vec::new(),

            ExecError::ExternSymbolLoadFailed { lib, symbol, path, msg, cause, .. } => {
                let mut notes = vec![
                    format!("Failed to load {lib}::{symbol} ({})", path.display()),
                    msg.to_string()
                ];
                if let Some(cause) = cause {
                    notes.push(cause.clone());
                };
                notes
            }
            ExecError::MarshalError(err) => vec![
                err.to_string()
            ],
            ExecError::IllegalDereference { ptr } => vec![
                format!("failed to dereference pointer: {}", ptr)
            ],
            ExecError::InternalError { msg } => vec![
                msg.clone()
            ],
            ExecError::NativeHeapError(err) => {
                err.notes()
            },
            ExecError::StackError(err) => vec![
                err.to_string()
            ],
            ExecError::IllegalInstruction(i) => vec![
                i.to_string()
            ],

            ExecError::WithStackTrace { err, stack_trace } => {
                let mut notes = err.notes();
                if !stack_trace.is_empty() {
                    notes.push(stack_trace.to_string());
                }
                notes
            },

            | ExecError::ZeroLengthAllocation => Vec::new(),
        }
    }
}

impl<Ty> From<MarshalError<Ty>> for ExecError<Ty> {
    fn from(value: MarshalError<Ty>) -> Self {
        match value {
            MarshalError::ExternSymbolLoadFailed { lib, symbol, path: filename, msg, cause } => {
                ExecError::ExternSymbolLoadFailed { lib, symbol, path: filename, msg, cause }
            }

            err => {
                ExecError::MarshalError(err)
            }
        }
    }
}

pub type ExecResult<T, Ty = ir::Type> = Result<T, ExecError<Ty>>;
