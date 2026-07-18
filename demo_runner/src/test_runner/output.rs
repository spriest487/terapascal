use std::path::PathBuf;
use crate::error::RunError;

#[derive(Debug)]
pub enum TestStatus {
    Running,
    Skipped,
    OK,
    Failed(FailureReason),
    Error(RunError),
}

impl TestStatus {
    pub fn is_ok(&self) -> bool {
        matches!(self, TestStatus::OK)
    }

    pub fn is_skipped(&self) -> bool {
        matches!(self, TestStatus::Skipped)
    }
}

#[derive(Debug)]
pub enum FailureReason {
    MissingOut(String),
    UnexpectedErr(String),
}

pub struct TestOutput {
    pub path: PathBuf,
    pub index: usize,

    pub status: TestStatus,

    pub log: String,
}

impl TestOutput {
    pub fn skipped(path: PathBuf, index: usize) -> Self {
        Self {
            path,
            index,

            status: TestStatus::Skipped,

            log: String::new(),
        }
    }
}