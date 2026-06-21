use crate::DiagnosticOutput;
use crate::Severity;

pub enum BuildLogEntry {
    Trace(String),
    Diagnostic(Box<dyn DiagnosticOutput>),
}

#[derive(Default)]
pub struct BuildLog {
    pub entries: Vec<BuildLogEntry>,
}

impl BuildLog {
    pub fn new() -> Self {
        BuildLog {
            entries: Vec::new(),
        }
    }

    pub fn trace(&mut self, message: impl Into<String>) {
        self.entries.push(BuildLogEntry::Trace(message.into()));
    }

    pub fn diagnostic(&mut self, diagnostic: impl DiagnosticOutput + 'static) {
        self.entries.push(BuildLogEntry::Diagnostic(Box::new(diagnostic)));
    }
    
    pub fn has_errors(&self) -> bool {
        self.entries.iter().any(|e| match e {
            BuildLogEntry::Trace(..) => false,
            BuildLogEntry::Diagnostic(diag) => diag.severity() >= Severity::Error,
        })
    }
}
