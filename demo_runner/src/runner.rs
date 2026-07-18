use crate::build::*;
use crate::concat_reader::ConcatReader;
use crate::error::RunError;
use crate::error::RunResult;
use crate::opts::*;
use crate::test_case::TestCase;
use crate::test_script::TestScriptStep;
use regex::Regex;
use std::env::consts::EXE_EXTENSION;
use std::fs;
use std::io;
use std::io::BufRead;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;
use std::process::ChildStderr;
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Output;
use std::process::Stdio;
use terapascal_common::IR_LIB_EXT;

fn run_vm<RunFn>(case: &TestCase, opts: &Opts, run: RunFn) -> RunResult<()>
where
    RunFn: FnOnce(&mut dyn Write, &mut dyn Read, &mut dyn Read)
{
    let mut build_stdout = Vec::new();
    let mut build_stderr = Vec::new();

    let lib_path = target_file_path(&case.path, opts, IR_LIB_EXT)?;

    if let Err(err) = compile_lib(case, &lib_path, opts, &mut build_stdout, &mut build_stderr) {
        run_after_build_err(&mut build_stdout, &mut build_stderr, run);
        return Err(err);
    };

    let mut run_command = find_command(&opts.compiler)?;
    run_command.arg(lib_path.canonicalize()?)
        .current_dir(case.working_dir());

    apply_compiler_args(&case, opts, &mut run_command);

    try_run_interactive(
        &mut run_command,
        opts,
        |stdin, stdout, stderr| {
            let mut stdout = ConcatReader::new(build_stdout.as_slice(), stdout);
            let mut stderr = ConcatReader::new(build_stderr.as_slice(), stderr);
            run(stdin, &mut stdout, &mut stderr)
        })?;

    Ok(())
}

fn run_dotnet<RunFn>(case: &TestCase, opts: &Opts, run: RunFn) -> RunResult<()>
where
    RunFn: FnOnce(&mut dyn Write, &mut dyn Read, &mut dyn Read)
{
    let mut build_stdout = Vec::new();
    let mut build_stderr = Vec::new();

    let dll_path = target_file_path(&case.path, opts, "dll")?;
    if is_dirty(&dll_path, &case.path, opts)? {
        if dll_path.exists() {
            fs::remove_file(&dll_path)?;
        }

        let mut build_command = find_command(&opts.compiler)?;
        build_command.arg(&case.path);
        build_command.arg("-o").arg(&dll_path);
        build_command.arg("-a").arg("cil");

        apply_compiler_args(&case, opts, &mut build_command);

        let build_status = try_run_command(
            &mut build_command,
            &mut build_stdout,
            &mut build_stderr
        )?;

        if !build_status.success() || !dll_path.exists() {
            run_after_build_err(&mut build_stdout, &mut build_stderr, run);
            return Err(RunError::BuildBinaryFailed);
        }
    }

    let mut run_command = Command::new("dotnet");
    run_command.arg(dll_path.canonicalize()?)
        .current_dir(case.working_dir());

    try_run_interactive(
        &mut run_command,
        opts,
        |stdin, stdout, stderr| {
            let mut stdout = ConcatReader::new(build_stdout.as_slice(), stdout);
            let mut stderr = ConcatReader::new(build_stderr.as_slice(), stderr);
            run(stdin, &mut stdout, &mut stderr)
        })?;

    Ok(())
}

fn build_clang(
    case: &TestCase,
    exe_path: &PathBuf,
    build_stdout: &mut Vec<u8>,
    build_stderr: &mut Vec<u8>,
    opts: &Opts
) -> RunResult<()> {
    if !is_dirty(exe_path, &case.path, opts)? {
        return Ok(());
    }

    let lib_path = target_file_path(&case.path, opts, IR_LIB_EXT)?;

    if let Err(err) = compile_lib(case, &lib_path, opts, build_stdout, build_stderr) {
        return Err(err);
    };

    let mut exe_file_path = exe_path.clone();
    exe_file_path.set_extension(EXE_EXTENSION);

    let mut compile_command = find_command(&opts.compiler)?;

    compile_command.arg(&lib_path);
    compile_command.arg("-o").arg(&exe_file_path);

    apply_compiler_args(&case, opts, &mut compile_command);

    let compile_status = try_run_command(
        &mut compile_command,
        build_stdout,
        build_stderr
    )?;

    if !compile_status.success() {
        return Err(RunError::BuildBinaryFailed);
    }

    Ok(())
}

fn run_clang<RunFn>(case: &TestCase, opts: &Opts, run: RunFn) -> RunResult<()>
where RunFn: FnOnce(&mut dyn Write, &mut dyn Read, &mut dyn Read)
{
    let exe_path = target_file_path(&case.path, opts, EXE_EXTENSION)?;

    let mut build_stdout = Vec::new();
    let mut build_stderr = Vec::new();

    if let Err(err) = build_clang(case, &exe_path, &mut build_stdout, &mut build_stderr, opts) {
        run_after_build_err(&mut build_stdout, &mut build_stderr, run);

        return Err(err);
    }

    try_run_interactive(
        find_command(&exe_path)?.current_dir(case.working_dir()),
        opts,
        |stdin, stdout, stderr| {
            let mut concat_stdout = ConcatReader::new(build_stdout.as_slice(), stdout);
            let mut concat_stderr = ConcatReader::new(build_stderr.as_slice(), stderr);

            run(stdin, &mut concat_stdout, &mut concat_stderr);
        },
    )?;

    Ok(())
}

fn try_run(case: &TestCase, opts: &Opts) -> io::Result<bool> {
    let mut io_error = false;
    let mut step_failed = false;

    let runner = match opts.exec {
        ExecutionMethod::Vm => run_vm,
        ExecutionMethod::Dotnet => run_dotnet,
        ExecutionMethod::Clang => run_clang,
    };

    let result = runner(case, opts, |stdin: &mut dyn Write, stdout: &mut dyn Read, stderr: &mut dyn Read| {
        let mut line_buf = Vec::new();
        for step in &case.script.steps {
            match run_step(step, &mut *stdin, stdout, stderr, &mut line_buf) {
                Ok(true) => {
                    continue;
                },

                Ok(false) => {
                    step_failed = true;
                    break;
                }

                Err(err) => {
                    println!("FAILED ({err})");

                    io_error = true;
                    break;
                }
            }
        }
    });

    let expect_error = case.script.steps.iter().any(|step| step.error_regex.is_some());

    let ok = !step_failed && !io_error && (result.is_ok() || expect_error);

    let completed = if ok {
        println!("OK");

        true
    } else {
        false
    };

    Ok(completed)
}

pub fn run(case: &TestCase, opts: &Opts) -> bool {
    let ok = try_run(case, opts).unwrap_or_else(|err| {
        println!("FAILED ({err})");
        false
    });

    println!();

    ok
}

fn run_step(
    step: &TestScriptStep,
    stdin: &mut dyn Write,
    stdout: &mut dyn Read,
    stderr: &mut dyn Read,
    line_buf: &mut Vec<u8>,
) -> io::Result<bool> {
    line_buf.clear();

    if let Some(err_pattern) = &step.error_regex {
        let err_regex = Regex::new(&format!("(?s){}", err_pattern))
            .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))?;
        
        // an error should be printed then the process should abort, so we can read the whole output
        stderr.read_to_end(line_buf)?;
        for line in line_buf.lines() {
            println!("  !! {}", line?);
        }

        let output = buf_to_string(line_buf.clone())?;
        
        let expected_err = err_regex.is_match(&output);
        if !expected_err {
            println!("ERROR: unexpected error (expected pattern: {})", err_regex);
        }
        
        return Ok(expected_err);
    }

    if let Some(input) = &step.input {
        println!("  >> {}", input.trim_end());
        stdin.write_all(input.as_bytes())?;
        stdin.flush()?;
    }

    let expect_output = step.output.is_some() || step.output_regex.is_some();
    if expect_output {
        let output_line = read_to_line_feed(stdout, line_buf)?;
        println!("  << {}", output_line);
        
        let (is_match, expected_pattern) = {
            if let Some(exact_output) = &step.output {
                let is_match = output_line.trim() == exact_output.trim();

                (is_match, exact_output.as_str())
            } else {
                let pattern = step.output_regex.as_ref().unwrap();
                let regex = Regex::new(&format!("(?s){}", pattern))
                    .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))?;
                
                let is_match = regex.is_match(&output_line);

                (is_match, pattern.as_str())
            }
        };

        if !is_match {
            println!("ERROR: unexpected output");
            println!("EXPECTED: {}", expected_pattern.trim_end());

            return Ok(false);
        }
    }

    Ok(true)
}

fn read_to_line_feed(mut src: impl Read, buf: &mut Vec<u8>) -> io::Result<String> {
    loop {
        let mut next = [0];
        src.read_exact(&mut next)?;

        if next[0] == b'\n' {
            break buf_to_string(buf.clone());
        }

        if next[0] != b'\r' {
            buf.push(next[0]);
        }
    }
}

fn buf_to_string(buf: Vec<u8>) -> io::Result<String> {
    match String::from_utf8(buf) {
        Ok(string) => Ok(string.trim_end().to_string()),
        Err(err) => {
            let msg = format!("unreadable output: {}", err);
            Err(io::Error::new(io::ErrorKind::InvalidData, msg))
        }
    }
}

fn try_run_interactive<RunFn>(command: &mut Command, opts: &Opts, f: RunFn) -> io::Result<ExitStatus>
    where RunFn: FnOnce(&mut ChildStdin, &mut ChildStdout, &mut ChildStderr)
{
    if opts.verbose {
        eprintln!("running command: {:?}", command);
    }

    let mut proc = command
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    let mut stdin = proc.stdin.take()
        .expect("stdin was not captured for child process");
    let mut stdout = proc.stdout.take()
        .expect("stdout was not captured for child process");
    let mut stderr = proc.stderr.take()
        .expect("stderr was not captured for child process");

    f(&mut stdin, &mut stdout, &mut stderr);

    proc.stdin = Some(stdin);
    proc.stdout = Some(stdout);
    proc.stderr = Some(stderr);

    let output = proc.wait_with_output()?;
    dump_output(&output);

    Ok(output.status)
}

// run the test on the output of a failed build
// (a failure might be OK, we need to verify some build errors log correctly)
fn run_after_build_err<RunFn>(stdout: &mut Vec<u8>, stderr: &mut Vec<u8>, run: RunFn)
where 
    RunFn: FnOnce(&mut dyn Write, &mut dyn Read, &mut dyn Read)
{
    let mut no_write = Vec::new();
    run(&mut no_write, &mut stdout.as_slice(), &mut stderr.as_slice());

    dump_output_buffers(&stdout, &stderr);
}

fn dump_output_buffers(stdout: &[u8], stderr: &[u8]) {
    let stdout = String::from_utf8_lossy(stdout);
    if stdout.len() > 0 {
        for line in stdout.lines() {
            println!("  << {}", line.trim_end());
        }
    }

    let stderr = String::from_utf8_lossy(stderr);
    if stderr.len() > 0 {
        for line in stderr.lines() {
            println!("  !! {}", line.trim_end());
        }
    }
}

fn dump_output(output: &Output) {
    dump_output_buffers(&output.stdout, &output.stderr);
}