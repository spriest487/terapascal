pub mod source_map;
pub mod span;
pub mod build_log;
pub mod fs;
pub mod aggregate_err;

use crate::span::*;
pub use backtrace::Backtrace;
use std::cmp::Ordering;
use std::collections::hash_map::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::ops::Deref;

pub const IR_LIB_EXT: &str = "lib";
pub const SRC_FILE_DEFAULT_EXT: &str = "tpas";

pub trait DiagnosticOutput: fmt::Display {
    fn title(&self) -> String {
        self.to_string()
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        None
    }

    fn notes(&self) -> Vec<String> {
        Vec::new()
    }

    fn main(&self, severity: Severity) -> DiagnosticMessage {
        let title = self.title();
        let label = self.label();
        let notes = self.notes().to_vec();

        DiagnosticMessage {
            severity,
            title,
            label,
            notes,
        }
    }
    
    fn to_messages(&self, main_severity: Severity) -> Vec<DiagnosticMessage> {
        let mut messages = self.see_also();
        messages.insert(0, self.main(main_severity));

        messages
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        Vec::new()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        None
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DiagnosticLabel {
    pub text: Option<String>,
    pub span: Span,
}

impl DiagnosticLabel {    
    pub fn new(span: Span) -> Self {
        Self { text: None, span }
    }

    pub fn with_text(mut self, text: impl Into<String>) -> Self {
        self.text = Some(text.into());
        self
    }
}

impl Ord for DiagnosticLabel {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.span.file.as_ref().cmp(other.span.file.as_ref()) {
            Ordering::Equal => match self.span.end.cmp(&other.span.end) {
                Ordering::Equal => self.span.start.cmp(&other.span.start),
                end_ord => end_ord,
            },
            file_ord => file_ord,
        }
    }
}

impl PartialOrd for DiagnosticLabel {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Severity {
    Help,
    Info,
    Warning,
    Error,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct DiagnosticMessage {
    pub severity: Severity,
    
    pub title: String,
    pub notes: Vec<String>,
    pub label: Option<DiagnosticLabel>,
}

impl DiagnosticMessage {
    pub fn new(severity: Severity, title: impl Into<String>) -> Self {
        Self {
            severity,
            title: title.into(),
            label: None,
            notes: Vec::new(),
        }
    }

    pub fn info(title: impl Into<String>) -> Self {
        Self::new(Severity::Info, title)
    }

    pub fn error(title: impl Into<String>) -> Self {
        Self::new(Severity::Warning, title)
    }

    pub fn warning(title: impl Into<String>) -> Self {
        Self::new(Severity::Error, title)
    }

    pub fn with_label(mut self, label: DiagnosticLabel) -> Self {
        self.label = Some(label);
        self
    }
    
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
}

impl Ord for DiagnosticMessage {
    fn cmp(&self, other: &Self) -> Ordering {
        self.label.cmp(&other.label)
    }
}

impl PartialOrd for DiagnosticMessage {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for DiagnosticMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.title)?;
        if let Some(label) = &self.label {
            writeln!(f)?;
            write!(f, "{}", label.span)?;
            if let Some(text) = &label.text {
                write!(f, ": {}", text)?;
            }
        }
        
        for note in &self.notes {
            writeln!(f)?;
            write!(f, " - {}", note)?;
        }
        
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct TracedError<T> {
    pub err: T,
    pub bt: Backtrace,
}

impl<T> TracedError<T> {
    fn trace_skip(err: T, skip_frames: usize) -> Self {
        let mut frames: Vec<_> = Backtrace::new().into();
        frames.rotate_left(skip_frames);
        frames.truncate(frames.len() - skip_frames);

        Self {
            err,
            bt: frames.into(),
        }
    }
    
    pub fn trace(err: T) -> Self {
        Self::trace_skip(err, 4)
    }

    pub fn chain<TNext: From<T>>(self) -> TracedError<TNext> {
        TracedError {
            err: self.err.into(),
            bt: self.bt,
        }
    }
    
    pub fn map(self, f: impl FnOnce(T) -> T) -> Self {
        TracedError {
            err: f(self.err),
            bt: self.bt,
        }
    }
    
    pub fn into_inner(self) -> T {
        self.err
    }
}

impl<T> From<T> for TracedError<T> {
    fn from(value: T) -> Self {
        Self::trace_skip(value, 4)
    }
}

impl<T> Deref for TracedError<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.err
    }
}

impl<T: fmt::Display> fmt::Display for TracedError<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.err)
    }
}

impl<T: DiagnosticOutput> DiagnosticOutput for TracedError<T> {
    fn title(&self) -> String {
        self.err.title()
    }

    fn label(&self) -> Option<DiagnosticLabel> {
        self.err.label()
    }

    fn notes(&self) -> Vec<String> {
        self.err.notes()
    }

    fn main(&self, severity: Severity) -> DiagnosticMessage {
        self.err.main(severity)
    }

    fn see_also(&self) -> Vec<DiagnosticMessage> {
        self.err.see_also()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        Some(&self.bt)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompileOpts {
    pub case_sensitive: bool,
    pub lang_mode: LanguageMode,

    pub verbose: bool,

    pp_symbols: HashSet<String>,
    switches: HashMap<String, bool>,
}

impl Default for CompileOpts {
    fn default() -> Self {
        CompileOpts {
            verbose: false,
            case_sensitive: true,
            lang_mode: LanguageMode::Default,
            pp_symbols: HashSet::new(),
            switches: HashMap::new(),
        }
    }
}

impl CompileOpts {
    pub fn defined(&self, pp_symbol: &str) -> bool {
        self.pp_symbols.contains(pp_symbol)
    }

    pub fn define(&mut self, pp_symbol: String) {
        self.pp_symbols.insert(pp_symbol);
    }

    pub fn undef(&mut self, sym: &str) -> bool {
        self.pp_symbols.remove(sym)
    }

    pub fn set_switch(&mut self, switch: &str, on: bool) {
        self.switches.insert(switch.to_string(), on);
    }

    pub fn is_switch_on(&self, switch: &str) -> bool {
        self.switches.get(switch).cloned().unwrap_or(false)
    }

    pub fn strict_switches(&self) -> bool {
        matches!(self.lang_mode, LanguageMode::Default)
    }

    pub fn link_lib(&mut self, _lib: String) {
        unimplemented!()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum LanguageMode {
    Default,
    Fpc,
    Delphi,
}

impl fmt::Display for LanguageMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LanguageMode::Default => "Default",
                LanguageMode::Fpc => "FPC-compatible",
                LanguageMode::Delphi => "Delphi-compatible",
            }
        )
    }
}

// pub fn path_relative_to_cwd(path: &Path) -> &Path {
//     env::current_dir()
//         .ok()
//         .and_then(|cwd| cwd.canonicalize().ok())
//         .and_then(|cwd| path.strip_prefix(cwd).ok())
//         .unwrap_or(path)
// }
