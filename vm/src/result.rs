use crate::heap::NativeHeapError;
use crate::ir;
use crate::marshal::MarshalError;
use crate::stack::StackError;
use crate::stack::StackTrace;
use crate::Pointer;
use std::fmt;
use terapascal_common::span::Span;
use terapascal_common::DiagnosticLabel;
use terapascal_common::DiagnosticOutput;
use terapascal_common::Severity;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ExecError<Ty: fmt::Display = ir::Type> {
    Raised {
        msg: String,
    },
    MarshalError(#[from] MarshalError<Ty>),
    StackError(#[from] StackError),
    ExternSymbolLoadFailed {
        msg: String,
        lib: String,
        symbol: String,
    },
    IllegalDereference {
        ptr: Pointer,
    },
    IllegalInstruction(ir::Instruction),
    IllegalState {
        msg: String,
    },
    NativeHeapError(#[from] NativeHeapError<Ty>),
    ZeroLengthAllocation,
    WithStackTrace {
        err: Box<ExecError<Ty>>,
        stack_trace: StackTrace,
    }
}

impl<Ty: fmt::Display> ExecError<Ty> {
    pub fn illegal_state(msg: impl Into<String>) -> Self {
        Self::IllegalState {
            msg: msg.into(),
        }
    }

    fn label_text(&self) -> Option<String> {
        match self {
            ExecError::Raised { msg } => Some(msg.clone()),
            ExecError::ExternSymbolLoadFailed { msg, .. } => Some(msg.clone()),
            ExecError::MarshalError(_err) => None,
            ExecError::IllegalDereference { ptr } => Some(format!("failed to dereference pointer: {}", ptr)),
            ExecError::IllegalState { msg } => Some(msg.clone()),
            ExecError::NativeHeapError(err) => Some(err.to_string()),
            ExecError::ZeroLengthAllocation => None,
            ExecError::StackError(err) => Some(err.to_string()),
            ExecError::IllegalInstruction(i) => Some(i.to_string()),
            ExecError::WithStackTrace { err, .. } => err.label_text(),
        }
    }

    fn label_span(&self) -> Option<Span> {
        match self {
            ExecError::WithStackTrace { stack_trace, .. } => {
                Some(stack_trace.top().to_span())
            },
            _ => None,
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
            ExecError::ExternSymbolLoadFailed { msg, lib, symbol } => {
                ExecError::ExternSymbolLoadFailed { msg, lib, symbol }
            },
            ExecError::IllegalDereference { ptr } => {
                ExecError::IllegalDereference { ptr }
            },
            ExecError::IllegalInstruction(illegal) => {
                ExecError::IllegalInstruction(illegal)
            },
            ExecError::IllegalState { msg } => {
                ExecError::IllegalState { msg }
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
        match self {
            ExecError::WithStackTrace { err, stack_trace, .. } => {
                write!(f, "{}", err)?;
                for stack_trace_line in stack_trace {
                    writeln!(f)?;
                    write!(f, "\tat {}", stack_trace_line)?;
                }
                Ok(())
            },
            _ => {
                match self {
                    ExecError::Raised { .. } => write!(f, "Runtime error raised"),
                    ExecError::MarshalError(err) => write!(f, "{err}"),
                    ExecError::StackError(err) => write!(f, "{}", err),
                    ExecError::ExternSymbolLoadFailed { lib, symbol, .. } => {
                        write!(f, "Failed to load {}::{}", lib, symbol)
                    }
                    ExecError::IllegalDereference { .. } => write!(f, "Illegal dereference"),
                    ExecError::IllegalState { .. } => write!(f, "Illegal vm state"),
                    ExecError::NativeHeapError(..) => write!(f, "Heap error"),
                    ExecError::ZeroLengthAllocation => write!(f, "Dynamic allocation with length 0"),
                    ExecError::IllegalInstruction(..) => write!(f, "Illegal instruction"),
                    ExecError::WithStackTrace { .. } => unreachable!(),
                }?;

                // if no spanned label will be shown, output the label text as part of the main message
                if let (Some(label_text), None) = (self.label_text(), self.label_span()) {
                    write!(f, ": {}", label_text)?;
                }

                Ok(())
            },
        }?;

        Ok(())
    }
}

impl<Ty: fmt::Display> DiagnosticOutput for ExecError<Ty> {
    fn severity(&self) -> Severity {
        Severity::Error
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        match self {
            ExecError::WithStackTrace { err, stack_trace } => {
                let label_text = err.label_text();

                Some(DiagnosticLabel {
                    text: label_text,
                    span: stack_trace.top().to_span()
                })
            },

            _ => None,
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            ExecError::WithStackTrace { .. } => Vec::new(),
            _ => self.label_text().map(|text| vec![text]).unwrap_or_else(Vec::new),
        }
    }
}

pub type ExecResult<T, Ty = ir::Type> = Result<T, ExecError<Ty>>;
