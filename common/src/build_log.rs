use std::fmt;
use crate::{DiagnosticOutput, Severity};

pub enum BuildLogEntry {
    Trace(String),
    Error(Box<dyn DiagnosticOutput>),
    Warn(Box<dyn DiagnosticOutput>),
}

impl fmt::Display for BuildLogEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BuildLogEntry::Trace(message) => {
                write!(f, "{message}")?;
            }

            BuildLogEntry::Warn(warning) => {
                write!(f, "{}", warning.main(Severity::Warning))?;
                for see_also in warning.see_also() {
                    writeln!(f)?;
                    write!(f, "{}", see_also)?;
                }
            }

            BuildLogEntry::Error(error) => {
                write!(f, "{}", error.main(Severity::Error))?;
                for see_also in error.see_also() {
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

    pub fn warn(&mut self, warning: impl DiagnosticOutput + 'static) {
        self.entries.push(BuildLogEntry::Warn(Box::new(warning)));
    }

    pub fn error(&mut self, error: impl DiagnosticOutput + 'static) {
        self.entries.push(BuildLogEntry::Error(Box::new(error)));
    }
    
    pub fn has_errors(&self) -> bool {
        self.entries.iter().any(|e| matches!(e, BuildLogEntry::Error(..)))
    }
}
