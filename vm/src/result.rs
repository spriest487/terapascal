use crate::heap::NativeHeapError;
use crate::ir;
use crate::marshal::MarshalError;
use crate::stack::StackError;
use crate::stack::StackTrace;
use crate::Pointer;
use common::span::Span;
use common::DiagnosticLabel;
use common::DiagnosticOutput;
use ir::InstructionFormatter;
use ir::RawInstructionFormatter;
use std::fmt;

#[derive(Debug)]
pub enum ExecError {
    Raised {
        msg: String,
    },
    MarshalError(MarshalError),
    StackError(StackError),
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
    NativeHeapError(NativeHeapError),
    ZeroLengthAllocation,
    WithStackTrace {
        err: Box<ExecError>,
        stack_trace: StackTrace,
    }
}

impl ExecError {
    pub fn illegal_state(msg: impl Into<String>) -> Self {
        Self::IllegalState {
            msg: msg.into(),
        }
    }

    fn label_text(&self) -> Option<String> {
        match self {
            ExecError::Raised { msg } => Some(msg.clone()),
            ExecError::ExternSymbolLoadFailed { msg, .. } => Some(msg.clone()),
            ExecError::MarshalError(err) => Some(err.to_string()),
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

    pub fn fmt_pretty<Pretty>(&self, f: &mut fmt::Formatter, pretty: &Pretty) -> fmt:: Result
    where
        Pretty: InstructionFormatter
    {
        match self {
            ExecError::WithStackTrace { err, stack_trace, .. } => {
                err.fmt_pretty(f, pretty)?;
                for stack_trace_line in stack_trace {
                    writeln!(f)?;
                    write!(f, "\tat {}", stack_trace_line)?;
                }
                Ok(())
            },
            _ => {
                match self {
                    ExecError::Raised { .. } => write!(f, "Runtime error raised"),
                    ExecError::MarshalError(err) => err.fmt_pretty(f, pretty),
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

impl fmt::Display for ExecError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_pretty(f, &RawInstructionFormatter)
    }
}

impl DiagnosticOutput for ExecError {
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

impl From<NativeHeapError> for ExecError {
    fn from(err: NativeHeapError) -> Self {
        ExecError::NativeHeapError(err)
    }
}

impl From<MarshalError> for ExecError {
    fn from(err: MarshalError) -> Self {
        ExecError::MarshalError(err)
    }
}

impl From<StackError> for ExecError {
    fn from(err: StackError) -> Self { ExecError::StackError(err) }
}

pub type ExecResult<T> = Result<T, ExecError>;
