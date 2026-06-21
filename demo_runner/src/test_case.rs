use crate::concat_reader::ConcatReader;
use crate::opts::ExecutionMethod;
use crate::opts::Opts;
use crate::test_script::TestScript;
use crate::test_script::TestScriptStep;
use regex::Regex;
use std::env;
use std::ffi::OsStr;
use std::fs;
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
                    .filter(|case| !case.script.ignore)
                    .collect()
            )
            .unwrap_or_default()
    }
    
    // run the test on the output of a failed build
    // (a failure might be OK, we need to verify some build errors log correctly)
    fn run_after_build_err<RunFn>(stdout: &mut Vec<u8>, stderr: &mut Vec<u8>, run: RunFn)
        where RunFn: FnOnce(&mut dyn Write, &mut dyn Read, &mut dyn Read)
    {
        let mut no_write = Vec::new();
        run(&mut no_write, &mut stdout.as_slice(), &mut stderr.as_slice());

        dump_output_buffers(&stdout, &stderr);
    }
    
    fn run_vm<RunFn>(&self, opts: &Opts, run: RunFn) -> io::Result<ExitStatus> 
        where RunFn: FnOnce(&mut dyn Write, &mut dyn Read, &mut dyn Read)
    {
        let mut build_stdout = Vec::new();
        let mut build_stderr = Vec::new();
        
        let module_path = target_file_path(&self.path, opts, "lib")?;
        // eprintln!("module path: {}", module_path.display());

        if is_dirty(&module_path, &self.path, opts)? {
            let mut build_command = Command::new(&opts.compiler);
            build_command.arg(&self.path);
            build_command.arg("-o").arg(&module_path);

            apply_compiler_args(&self.script, opts, &mut build_command);

            let build_status = try_run_command(
                &mut build_command,
                &mut build_stdout,
                &mut build_stderr
            )?;

            if !build_status.success() {
                Self::run_after_build_err(&mut build_stdout, &mut build_stderr, run);
                return Ok(build_status);
            }
        }

        let mut run_command = find_command(&opts.compiler)?;
        run_command.arg(module_path.canonicalize()?)
            .current_dir(self.working_dir());

        // never run in verbose mode, it affects the output
        if opts.verbose {
            let mut run_opts = opts.clone();
            run_opts.verbose = false;
            apply_compiler_args(&self.script, &run_opts, &mut run_command);
        } else {
            apply_compiler_args(&self.script, opts, &mut run_command);
        };

        try_run_interactive(
            &mut run_command,
            opts,
            |stdin, stdout, stderr| {
                let mut stdout = ConcatReader::new(build_stdout.as_slice(), stdout);
                let mut stderr = ConcatReader::new(build_stderr.as_slice(), stderr);
                run(stdin, &mut stdout, &mut stderr)
            })
    }

    fn run_dotnet<RunFn>(&self, opts: &Opts, run: RunFn) -> io::Result<ExitStatus>
        where RunFn: FnOnce(&mut dyn Write, &mut dyn Read, &mut dyn Read)
    {
        let mut build_stdout = Vec::new();
        let mut build_stderr = Vec::new();

        let dll_path = target_file_path(&self.path, opts, "dll")?;
        if is_dirty(&dll_path, &self.path, opts)? {
            if dll_path.exists() {
                fs::remove_file(&dll_path)?;
            }

            let mut build_command = find_command(&opts.compiler)?;
            build_command.arg(&self.path);
            build_command.arg("-o").arg(&dll_path);
            build_command.arg("-a").arg("cil");

            apply_compiler_args(&self.script, opts, &mut build_command);

            let build_status = try_run_command(
                &mut build_command,
                &mut build_stdout,
                &mut build_stderr
            )?;

            if !build_status.success() || !dll_path.exists() {
                Self::run_after_build_err(&mut build_stdout, &mut build_stderr, run);
                return Ok(build_status);
            }
        }

        let mut run_command = Command::new("dotnet");
        run_command.arg(dll_path.canonicalize()?)
            .current_dir(self.working_dir());

        try_run_interactive(
            &mut run_command,
            opts,
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
        if !is_dirty(exe_path, &self.path, opts)? {
            return Ok(None);
        }

        let mut c_file_path = exe_path.clone();
        c_file_path.set_extension("c");

        let mut compile_command = find_command(&opts.compiler)?;

        compile_command.arg(&self.path);
        compile_command.arg("-o").arg(&c_file_path);

        apply_compiler_args(&self.script, opts, &mut compile_command);

        let compile_status = try_run_command(
            &mut compile_command,
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
                .arg("-Werror")
                .arg("-Wall")
                .arg("-Wextra")
                .arg("-Wno-unused-function")
                .arg("-Wno-unused-parameter")
                .arg("-Wno-unused-variable")
                .arg("-Wno-unused-label")
                .arg("-Wno-address-of-packed-member")
                .arg("-Wno-parentheses-equality")
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
            find_command(&exe_path)?.current_dir(self.working_dir()),
            opts,
            |stdin, stdout, stderr| {
                let mut concat_stdout = ConcatReader::new(build_stdout.as_slice(), stdout);
                let mut concat_stderr = ConcatReader::new(build_stderr.as_slice(), stderr);

                run(stdin, &mut concat_stdout, &mut concat_stderr);
            },
        )
    }
    
    fn try_run(&self, opts: &Opts) -> io::Result<bool> {
        let mut io_error = false;
        let mut step_failed = false;
        
        let runner = match opts.exec {
            ExecutionMethod::Vm => Self::run_vm,
            ExecutionMethod::Dotnet => Self::run_dotnet,
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
                .unwrap_or_else(|| String::from("(UNKNOWN)"));

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

fn apply_compiler_args(script: &TestScript, opts: &Opts, compiler_command: &mut Command) {
    if opts.verbose {
        compiler_command.arg("-v");
    }

    if opts.debug {
        compiler_command.arg("-g");
    }

    for extra_package in &script.packages {
        compiler_command.arg("-p").arg(extra_package);
    }
}

fn try_run_command(command: &mut Command, stdout: &mut Vec<u8>, stderr: &mut Vec<u8>) -> io::Result<ExitStatus> {
    let mut output = command.output()?;

    stdout.append(&mut output.stdout);
    stderr.append(&mut output.stderr);

    Ok(output.status)
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

fn find_command(command: &Path) -> io::Result<Command> {
    if command.is_relative() {
        let command = env::current_dir()?.join(command);
        Ok(Command::new(command))
    } else {
        Ok(Command::new(command))
    }
}

fn is_dirty(target: &Path, source: &Path, opts: &Opts) -> io::Result<bool> {
    if opts.clean {
        return Ok(true);
    }
    
    if !target.exists() {
        return Ok(true);
    }

    let compiler_path = find_command(&opts.compiler)?
        .get_program()
        .to_os_string();

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
        (Some(target_modified), Some(source_modified)) => {
            Ok(source_modified > target_modified)
        },
        _ => {
            Ok(true)
        },
    }
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
