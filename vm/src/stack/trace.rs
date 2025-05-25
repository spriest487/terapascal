use common::span::{Location, Span};
use serde::Serialize;
use std::fmt::Formatter;
use std::{fmt, slice};

#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct StackTrace {
    frames: Vec<StackTraceFrame>,     
}

impl StackTrace {
    pub fn new(frames: impl IntoIterator<Item=StackTraceFrame>) -> Self {
        let mut stack_trace = Self { frames: frames.into_iter().collect() };
        
        if stack_trace.frames.is_empty() {
            stack_trace.frames.push(StackTraceFrame {
                file: String::new(),
                start_loc: Location::zero(),
                end_loc: Location::zero(),
                name: String::new(),
            });
        }
        
        stack_trace
    }
    
    pub fn top(&self) -> &StackTraceFrame {
        &self.frames[0]
    }
    
    pub fn frames(&self) -> impl Iterator<Item=&StackTraceFrame> {
        self.frames.iter()
    }
    
    pub fn is_empty(&self) -> bool {
        self.frames.len() == 1 && self.frames[0].name == ""
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

            write!(f, "{}", self.frames[i])?;
        }
        
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct StackTraceFrame {
    pub name: String,
    pub file: String,
    pub start_loc: Location,
    pub end_loc: Location,
}

impl StackTraceFrame {
    pub fn new(name: String, span: &Span) -> Self {
        Self {
            name,
            file: span.file.display().to_string(),
            start_loc: span.start,
            end_loc: span.end,
        }
    }
    
    pub fn to_span(&self) -> Span {
        Span::new(&self.name, self.start_loc, self.end_loc)
    }
}

impl fmt::Display for StackTraceFrame {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({}:{})", self.name, self.file, self.start_loc)
    }
}
