use std::io;
use std::io::Write;
use std::process::*;
use crate::error::{RunError, RunResult};

pub struct TestProcess {
    pub process: Child,
    stdin: Option<ChildStdin>,
    pub stdout: ChildStdout,
    pub stderr: ChildStderr,
}

impl TestProcess {
    pub fn from_process(mut process: Child) -> Self {
        TestProcess {
            stdin: process.stdin.take(),
            stdout: process.stdout.take().unwrap(),
            stderr: process.stderr.take().unwrap(),
            process,
        }
    }

    pub fn wait(mut self, out_buf: &mut Vec<u8>, err_buf: &mut Vec<u8>) -> RunResult<()> {
        self.process.stdin = self.stdin;
        self.process.stdout = Some(self.stdout);
        self.process.stderr = Some(self.stderr);

        let mut output = self.process.wait_with_output()?;
        out_buf.append(&mut output.stdout);
        err_buf.append(&mut output.stderr);

        if !output.status.success() {
            return Err(RunError::ErrorCode(output.status.to_string()));
        }

        Ok(())
    }

    pub fn close_stdin(&mut self) {
        self.stdin = None;
    }

    pub fn write_stdin(&mut self, bytes: &[u8]) -> RunResult<()> {
        let Some(stdin) = &mut self.stdin else {
            let msg = "test process stdin was already closed";
            let err = io::Error::new(io::ErrorKind::BrokenPipe, msg);

            return Err(RunError::from(err));
        };

        stdin.write(bytes)?;
        Ok(())
    }
}
