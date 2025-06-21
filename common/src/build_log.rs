use crate::{DiagnosticOutput, Severity};
use std::fmt;

pub enum BuildLogEntry {
    Trace(String),
    Diagnostic(Box<dyn DiagnosticOutput>),
}

impl fmt::Display for BuildLogEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BuildLogEntry::Trace(message) => {
                write!(f, "[trace] {message}")?;
            }

            BuildLogEntry::Diagnostic(diagnostic) => {
                write!(f, "[{}]", diagnostic.severity().to_string().to_lowercase())?;
                
                write!(f, "{}", diagnostic.main())?;
                for see_also in diagnostic.see_also() {
                    writeln!(f)?;
                    write!(f, "{}", see_also)?;
                }
            }
        }

        Ok(())
    }
}

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

    pub fn diagnostic(&mut self, warning: impl DiagnosticOutput + 'static) {
        self.entries.push(BuildLogEntry::Diagnostic(Box::new(warning)));
    }
    
    pub fn has_errors(&self) -> bool {
        self.entries.iter().any(|e| match e {
            BuildLogEntry::Trace(..) => false,
            BuildLogEntry::Diagnostic(diag) => diag.severity() >= Severity::Error,
        })
    }
}
