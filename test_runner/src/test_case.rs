use crate::concat_reader::ConcatReader;
use crate::opts::ExecutionMethod;
use crate::opts::Opts;
use crate::test_script::TestScript;
use crate::test_script::TestScriptStep;
use regex::Regex;
use std::env;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::fs::DirEntry;
use std::io;
use std::io::BufRead;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::ChildStderr;
use std::process::ChildStdin;
use std::process::ChildStdout;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Output;
use std::process::Stdio;
use std::time::SystemTime;
use terapascal_common::SRC_FILE_DEFAULT_EXT;

#[derive(Clone)]
pub struct TestCase {
    pub path: PathBuf,
    pub script: TestScript,
}

impl TestCase {
    fn find_from_entry(entry: DirEntry) -> io::Result<Vec<TestCase>> {
        let file_type = entry.file_type()?;

        let paths = if file_type.is_dir() {
            TestCase::find_at_path(&entry.path())
        } else {
            let path = entry.path();

            match path.extension() {
                Some(ext) if ext.eq_ignore_ascii_case(SRC_FILE_DEFAULT_EXT) => {
                    let script = TestScript::find_for_path(&path)
                        .unwrap_or_else(|err| {
                            eprintln!("failed to read test script: {err}");
                            TestScript::default()
                        });

                    if script.ignore {
                        Vec::new()
                    } else {
                        vec![TestCase { path, script }] 
                    }
                }

                _ => Vec::new(),
            }
        };

        Ok(paths)
    }

    pub fn find_at_path(root: &Path) -> Vec<TestCase> {
        root.read_dir()
            .ok()
            .map(|read_dir|
                read_dir
                    .filter_map(|read_entry| {
                        read_entry
                            .and_then(TestCase::find_from_entry)
                            .ok()
                    })
                    .flatten()
                    .collect()
            )
            .unwrap_or_default()
    }
    
    fn run_interpreted<RunFn>(&self, opts: &Opts, run: RunFn) -> io::Result<ExitStatus> 
        where RunFn: FnOnce(&mut dyn Write, &mut dyn Read, &mut dyn Read)
    {        
        let mut build_stdout = Vec::new();
        let mut build_stderr = Vec::new();
        
        let module_path = target_file_path(&self.path, opts, "lib")?;
        if is_target_outdated(&module_path, &self.path, opts) {
            let build_status = try_run_command(
                Command::new(&opts.compiler)
                    .arg(&self.path)
                    .arg("-o").arg(&module_path),
                &mut build_stdout,
                &mut build_stderr
            )?;

            if !build_status.success() {
                let mut no_write = Vec::new();
                run(&mut no_write, &mut build_stdout.as_slice(), &mut build_stderr.as_slice());

                dump_output_buffers(&build_stdout, &build_stderr);

                return Ok(build_status);
            }
        }

        try_run_interactive(
            Command::new(&opts.compiler)
                .arg(module_path.canonicalize()?)
                .current_dir(self.working_dir()),
            |stdin, stdout, stderr| {
                let mut stdout = ConcatReader::new(build_stdout.as_slice(), stdout);
                let mut stderr = ConcatReader::new(build_stderr.as_slice(), stderr);
                run(stdin, &mut stdout, &mut stderr)
            })
    }
    
    fn working_dir(&self) -> &Path {
        self.path.parent().expect("source file must have a parent directory")
    }
    
    fn build_clang(&self,
        exe_path: &PathBuf,
        build_stdout: &mut Vec<u8>,
        build_stderr: &mut Vec<u8>,
        opts: &Opts
    ) -> io::Result<Option<ExitStatus>> {
        if !is_target_outdated(exe_path, &self.path, opts) {
            return Ok(None);
        }
        
        let mut c_file_path = exe_path.clone();
        c_file_path.set_extension("c");

        let compile_status = try_run_command(
            Command::new(&opts.compiler)
                .arg(&self.path)
                .arg("-o").arg(&c_file_path),
            build_stdout,
            build_stderr
        )?;

        if !compile_status.success() {
            return Ok(Some(compile_status)); 
        }
        
        let mut clang_args = Vec::new();
        if opts.clang_debug || opts.clang_codeview {
            clang_args.push(OsStr::new("-g"));
            clang_args.push(OsStr::new("-O0"));
        }
        if opts.clang_codeview {
            clang_args.push(OsStr::new("-gcodeview"));
        }

        let clang_status = try_run_command(
            Command::new("clang")
                .arg(c_file_path)
                .arg("-o").arg(exe_path)
                .args(clang_args),
            build_stdout,
            build_stderr
        )?;
        
        if !clang_status.success() {
            return Ok(Some(clang_status));
        }

        Ok(None)
    }

    fn run_clang<RunFn>(&self, opts: &Opts, run: RunFn) -> io::Result<ExitStatus>
        where RunFn: FnOnce(&mut dyn Write, &mut dyn Read, &mut dyn Read)
    {
        let exe_path = target_file_path(&self.path, opts, env::consts::EXE_EXTENSION)?;

        let mut build_stdout = Vec::new();
        let mut build_stderr = Vec::new();
                
        if let Some(err_status) = self.build_clang(&exe_path, &mut build_stdout, &mut build_stderr, opts)? {
            let mut no_write = Vec::new();
            run(&mut no_write, &mut build_stdout.as_slice(), &mut build_stderr.as_slice());
            
            dump_output_buffers(&build_stdout, &build_stderr);
            
            return Ok(err_status);
        }

        try_run_interactive(
            Command::new(exe_path)
                .current_dir(self.working_dir()),
            |stdin, stdout, stderr| {
                let mut concat_stdout = ConcatReader::new(build_stdout.as_slice(), stdout);
                let mut concat_stderr = ConcatReader::new(build_stderr.as_slice(), stderr);

                run(stdin, &mut concat_stdout, &mut concat_stderr);
            }
        )
    }
    
    fn try_run(&self, opts: &Opts) -> io::Result<bool> {
        println!("RUNNING: {}", self.path.display());

        let mut io_error = false;
        let mut step_failed = false;
        
        let runner = match opts.exec {
            ExecutionMethod::Interpret => Self::run_interpreted,
            ExecutionMethod::Clang => Self::run_clang,
        };
        
        let status = runner(self, opts, |stdin: &mut dyn Write, stdout: &mut dyn Read, stderr: &mut dyn Read| {
            let mut line_buf = Vec::new();
            for step in &self.script.steps {
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
        })?;
        
        let expect_error = self.script.steps.iter().any(|step| step.error_regex.is_some());
        let is_error = !status.success();

        let ok = !step_failed && !io_error && (!is_error || expect_error);

        let completed = if ok {
            println!("OK");

            true
        } else {
            let code = status
                .code()
                .map(|code| code.to_string())
                .unwrap_or_else(|| String::from("no return code"));

            println!("ERROR {}", code);

            false
        };

        Ok(completed)
    }
    
    pub fn run(&self, opts: &Opts) -> bool {
        let ok = self.try_run(opts).unwrap_or_else(|err| {
            println!("FAILED ({err})");
            false
        });
        
        println!();
        
        ok
    }
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

    if let Some(output) = &step.output {
        let line = read_to_line_feed(stdout, line_buf)?;
        println!("  << {}", line);

        if line != *output {
            println!("ERROR: unexpected output");
            println!("EXPECTED: {}", output.trim_end());

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

fn try_run_command(command: &mut Command, stdout: &mut Vec<u8>, stderr: &mut Vec<u8>) -> io::Result<ExitStatus> {
    let mut output = command.output()?;

    stdout.append(&mut output.stdout);
    stderr.append(&mut output.stderr);

    Ok(output.status)
}

fn try_run_interactive<RunFn>(command: &mut Command, f: RunFn) -> io::Result<ExitStatus> 
    where RunFn: FnOnce(&mut ChildStdin, &mut ChildStdout, &mut ChildStderr)
{
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

fn target_file_path(src_path: &Path, opts: &Opts, target_ext: impl AsRef<OsStr>) -> io::Result<PathBuf> {
    let path_as_rel = match src_path.strip_prefix(&opts.search_path) {
        Ok(path) => path,
        Err(err) => {
            return Err(io::Error::new(io::ErrorKind::Other, err.to_string()));
        }
    };

    let mut target_path = opts.target_path.join(&path_as_rel);
    target_path.set_extension(target_ext);
    
    Ok(target_path)
}

fn is_target_outdated(target: &Path, source: &Path, opts: &Opts) -> bool {
    if opts.clean {
        return true;
    }
    
    if !target.exists() {
        return true;
    }
    
    let compiler_path = OsString::from(Command::new(&opts.compiler).get_program());

    let target_timestamp = target.metadata()
        .and_then(|meta| meta.modified()).ok();

    let compiler_modified = Path::new(&compiler_path).metadata()
        .and_then(|meta| meta.modified()).ok();

    let get_source_modified = source.metadata()
        .and_then(|meta| meta.modified()).ok();
    
    let modified_timestamp = match (compiler_modified, get_source_modified) {
        (Some(compiler_time), Some(src_time)) => Some(SystemTime::max(compiler_time, src_time)),
        (Some(time), None) | (None, Some(time)) => Some(time),
        (None, None) => None,
    };

    match (target_timestamp, modified_timestamp) {
        (Some(target_modified), Some(source_modified)) =>
            source_modified > target_modified,
        _ => true,
    }
}

fn dump_output_buffers(stdout: &[u8], stderr: &[u8]) {
    let stdout = String::from_utf8_lossy(stdout);
    if stdout.len() > 0 {
        for line in stdout.lines() {
            println!("  << {}", line.trim());
        }
    }

    let stderr = String::from_utf8_lossy(stderr);
    if stderr.len() > 0 {
        for line in stderr.lines() {
            println!("  !! {}", line.trim());
        }
    }
}

fn dump_output(output: &Output) {
    dump_output_buffers(&output.stdout, &output.stderr);
}
