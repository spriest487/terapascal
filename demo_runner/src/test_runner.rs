mod test_process;

use self::test_process::*;
use crate::build::apply_compiler_args;
use crate::build::compile_lib;
use crate::build::find_command;
use crate::build::target_file_path;
use crate::error::RunError;
use crate::error::RunResult;
use crate::opts::*;
use crate::test_case::TestCase;
use crate::test_output::*;
use crate::util::*;
use regex::Regex;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;
use std::process::Stdio;
use terapascal_common::IR_LIB_EXT;

enum StepStatus {
    Passed,
    Failed(FailureReason),
    ErrMatched,
}

fn spawn_vm(case: &TestCase, lib_path: &Path, opts: &Opts) -> RunResult<TestProcess> {
    let mut compiler_command = find_command(&opts.compiler)?;

    // use the canonical lib path because we're setting the cwd for the VM
    let lib_path = lib_path.canonicalize()?;
    compiler_command.arg(lib_path);

    apply_compiler_args(&case, opts, &mut compiler_command);

    let child = compiler_command
        .current_dir(case.working_dir())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    Ok(TestProcess::from_process(child))
}

pub struct TestRunner<'a> {
    output: TestOutput,

    opts: &'a Opts,

    case: TestCase,
    step: usize,

    // per-step buffers
    out_buf: Vec<u8>,
    err_buf: Vec<u8>,
}

impl<'a> TestRunner<'a> {
    pub fn new(opts: &'a Opts, index: usize, case: TestCase) -> Self {
        Self {
            output: TestOutput {
                index,
                path: case.path.clone(),

                status: TestStatus::Running,

                log: String::new(),
            },

            opts,

            case,
            step: 0,

            out_buf: Vec::new(),
            err_buf: Vec::new(),
        }
    }

    pub fn run(mut self) -> TestOutput {
        let spawn_fn = match self.opts.exec {
            ExecutionMethod::Vm => spawn_vm,
            _ => todo!(),
        };

        let lib_path = match self.build_lib() {
            Ok(child) => {
                // if the build printed any output but still succeeded, ignore it
                self.flush_to_log();
                child
            }

            Err(err) => {
                return self.handle_error(err);
            }
        };

        let mut process = match spawn_fn(&self.case, &lib_path, &self.opts) {
            Ok(child) => child,

            Err(err) => {
                return self.handle_error(err);
            },
        };

        let mut failure_reason = None;
        let mut handled_error = false;

        while self.step < self.case.script.steps.len()  {
            let step_status = match self.run_step(&mut process) {
                Ok(status) => status,
                Err(err) => {
                    return self.handle_error(err);
                }
            };

            match step_status {
                StepStatus::Passed => {
                    self.flush_to_log();
                },
                StepStatus::ErrMatched => {
                    handled_error = true;
                    break;
                }
                StepStatus::Failed(fail) => {
                    handled_error = true;
                    failure_reason = Some(fail);
                    break;
                },
            };
        }

        // there's output remaining that no step matched, report it as an error now
        if failure_reason.is_none() {
            process.close_stdin();

            if let Err(err) = process.stdout.read_to_end(&mut self.out_buf) {
                return self.handle_error(err);
            }

            let out_text = String::from_utf8_lossy(&self.out_buf);
            if !out_text.is_empty() {
                failure_reason = Some(FailureReason::UnexpectedOut);
                handled_error = true;
            }
        }

        // if a step failed, we expect the process to exit with an error status, and that
        // error should not affect the outcome
        if let Err(err) = process.wait(&mut self.out_buf, &mut self.err_buf)
            && !handled_error
        {
            return self.handle_error(err);
        }

        self.flush_to_log();

        self.output.status = match failure_reason {
            None => TestStatus::OK,
            Some(reason) => TestStatus::Failed(reason),
        };

        self.output
    }

    fn flush_to_log(&mut self) {
        let out_text = String::from_utf8_lossy(&self.out_buf);
        for line in out_text.lines() {
            self.output.log.push_str("<< ");
            self.output.log.push_str(line);
            self.output.log.push_str("\n");
        }

        let err_text = String::from_utf8_lossy(&self.err_buf);
        for line in err_text.lines() {
            self.output.log.push_str("!! ");
            self.output.log.push_str(line);
            self.output.log.push_str("\n");
        }

        self.out_buf.clear();
        self.err_buf.clear();
    }

    fn run_step(&mut self, process: &mut TestProcess) -> RunResult<StepStatus> {
        let current_step = &self.case.script.steps[self.step];

        // 1. if input is provided for this step, send it
        if let Some(input) = &current_step.input {
            process.write_stdin(input.as_bytes())?;
        }

        // 2. if output is expected for this step, the out buf contents must match it,
        // and the test fails if there is no output
        if let Some(expect_output_pattern) = &current_step.output_regex {
            read_to_line_feed(&mut process.stdout, &mut self.out_buf)?;
            let line_text = String::from_utf8_lossy(&self.out_buf);

            if line_text.is_empty() {
                let failure = FailureReason::MissingOut(expect_output_pattern.clone());
                return Ok(StepStatus::Failed(failure));
            }

            let regex = Regex::new(&format!("(?s){}", expect_output_pattern))
                .map_err(|err| RunError::InvalidRegex {
                    err,
                    src: expect_output_pattern.clone(),
                })?;

            if !regex.is_match(&line_text) {
                return Ok(StepStatus::Failed(FailureReason::UnexpectedOut));
            }

            self.out_buf.push('\n' as u8);
        } else if let Some(expect_output) = &current_step.output {
            read_to_line_feed(&mut process.stdout, &mut self.out_buf)?;
            let line_text = String::from_utf8_lossy(&self.out_buf);

            if line_text.is_empty() {
                let failure = FailureReason::MissingOut(expect_output.clone());
                return Ok(StepStatus::Failed(failure));
            }

            if *expect_output != line_text {
                return Ok(StepStatus::Failed(FailureReason::UnexpectedOut));
            }

            self.out_buf.push('\n' as u8);
        }

        // 3. if an error is expected, read the entire remaining contents of stderr and compare it
        if let Some(expect_err) = &current_step.error_regex {
            process.close_stdin();
            process.stderr.read_to_end(&mut self.err_buf)?;

            let err_text = String::from_utf8_lossy(&self.err_buf);

            let status = if self.is_expected_error(expect_err, &err_text)? {
                StepStatus::ErrMatched
            } else {
                StepStatus::Failed(FailureReason::UnexpectedErr(err_text.to_string()))
            };

            return Ok(status)
        }

        self.step += 1;
        Ok(StepStatus::Passed)
    }

    fn is_expected_error(&self, pattern: &str, text: &str) -> RunResult<bool> {
        let err_regex = match Regex::new(&format!("(?s){}", pattern)) {
            Ok(pattern) => pattern,
            Err(err) => {
                return Err(RunError::InvalidRegex {
                    src: pattern.to_string(),
                    err,
                });
            }
        };

        let is_match = err_regex.is_match(text) || {
            let err_text = String::from_utf8_lossy(&self.err_buf);
            err_regex.is_match(&err_text)
        };

        Ok(is_match)
    }

    // handle unexpected errors e.g. anything except test step assertions
    fn handle_error(mut self, err: impl Into<RunError>) -> TestOutput {
        let mut is_expected_err = false;

        // when a failure occurs, check if either the step output or step error output matches
        // the expected error pattern - if it does, the test succeeds
        if let Some(err_pattern) = self.case.script.steps
            .get(self.step)
            .and_then(|step| step.error_regex.as_ref())
        {
            let out_text = String::from_utf8_lossy(&self.out_buf);

            is_expected_err = match self.is_expected_error(err_pattern, &out_text) {
                Ok(expected) => expected,
                Err(err) => {
                    self.output.status = TestStatus::Error(err);
                    return self.output;
                }
            };
        }

        self.flush_to_log();

        if is_expected_err {
            self.output.status = TestStatus::OK;
        } else {
            self.output.status = TestStatus::Error(err.into());
        }

        self.output
    }

    fn build_lib(&mut self) -> RunResult<PathBuf> {
        let lib_path = target_file_path(&self.case.path, &self.opts, IR_LIB_EXT)?;

        compile_lib(&self.case, &lib_path, &self.opts, &mut self.out_buf, &mut self.err_buf)?;

        Ok(lib_path)
    }
}