use serde::Serialize;
use std::fmt;
use std::fmt::Formatter;
use std::slice;
use terapascal_common::span::Span;

#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct StackTrace {
    frames: Vec<StackTraceFrame>,
}

impl StackTrace {
    pub fn new(frames: impl IntoIterator<Item = StackTraceFrame>) -> Self {
        let mut stack_trace = Self {
            frames: frames.into_iter().collect(),
        };

        if stack_trace.frames.is_empty() {
            stack_trace.frames.push(StackTraceFrame {
                span: None,
                name: String::new(),
            });
        }

        stack_trace
    }

    pub fn frames(&self) -> impl Iterator<Item = &StackTraceFrame> {
        self.frames.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.frames.len() == 1 && self.frames[0].name == ""
    }

    pub fn top_span(&self) -> Option<Span> {
        self.frames.iter()
            .find_map(|frame| frame.span.as_ref().cloned())
    }
}

impl<'a> IntoIterator for &'a StackTrace {
    type Item = &'a StackTraceFrame;
    type IntoIter = slice::Iter<'a, StackTraceFrame>;

    fn into_iter(self) -> Self::IntoIter {
        self.frames.iter()
    }
}

impl fmt::Display for StackTrace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            return Ok(());
        }

        for i in 0..self.frames.len() {
            if i > 0 {
                writeln!(f)?;
            }

            write!(f, "at {}", self.frames[i])?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct StackTraceFrame {
    pub name: String,
    pub span: Option<Span>,
}

impl StackTraceFrame {
    pub fn new(name: String) -> Self {
        Self {
            name,
            span: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
}

impl fmt::Display for StackTraceFrame {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(span) = &self.span {
            write!(f, " ({}:{})", span.file.display(), span.start)?;
        }
        Ok(())
    }
}
