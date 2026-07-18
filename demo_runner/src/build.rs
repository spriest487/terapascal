use crate::error::RunError;
use crate::error::RunResult;
use crate::opts::Opts;
use crate::test_case::TestCase;
use chrono::DateTime;
use chrono::Utc;
use std::env;
use std::env::consts::EXE_EXTENSION;
use std::ffi::OsStr;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitStatus;
use std::time::SystemTime;
use terapascal_common::IR_LIB_EXT;

pub fn check_environment(opts: &Opts) -> bool {
    let compiler_exists = opts.compiler.exists();
    if !compiler_exists {
        eprintln!("frontend not found! expected at {}", opts.compiler.display());
        return false;
    }

    println!("using frontend: {}", opts.compiler.display());

    let timestamp = match opts.compiler.metadata().and_then(|metadata| metadata.modified()) {
        Ok(modified) => {
            let modified_date = DateTime::<Utc>::from(modified);
            format!("{}", modified_date.format("%Y-%m-%d %H:%M"))
        }
        Err(err) => {
            eprintln!("unable to read compiler timestamp: {}", err);
            "unknown timestamp".to_string()
        },
    };

    let Some(version_check_out) = Command::new(opts.compiler.clone())
        .arg("--version")
        .output()
        .map(Some)
        .unwrap_or_else(|err| {
            eprintln!("version check failed: {}", err);
            None
        })
    else {
        return false;
    };

    println!("{} ({})", String::from_utf8(version_check_out.stdout).unwrap().trim(), timestamp);

    true
}

pub fn compile_lib(
    case: &TestCase,
    lib_path: &Path,
    opts: &Opts,
    build_stdout: &mut Vec<u8>,
    build_stderr: &mut Vec<u8>,
) -> RunResult<()> {
    // eprintln!("module path: {}", module_path.display());
    if is_dirty(lib_path, &case.path, opts)? {
        let mut build_command = Command::new(&opts.compiler);
        build_command.arg(&case.path);
        build_command.arg("-o").arg(lib_path);

        apply_compiler_args(&case, opts, &mut build_command);

        let build_status = try_run_command(
            &mut build_command,
            build_stdout,
            build_stderr
        )?;

        if !build_status.success() {
            return Err(RunError::BuildLibFailed);
        }
    }

    Ok(())
}

pub fn build_clang(
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
        return Err(RunError::BuildBinaryFailed(compile_status.to_string()));
    }

    Ok(())
}

pub fn target_file_path(src_path: &Path, opts: &Opts, target_ext: impl AsRef<OsStr>) -> io::Result<PathBuf> {
    if src_path.is_file() {
        let filename = src_path
            .file_name()
            .ok_or_else(|| {
                let srr = format!("unable to determine filename of path: {}", src_path.display());
                io::Error::new(io::ErrorKind::InvalidFilename, srr)
            })?;
        let rel_file_path = PathBuf::from(filename).with_extension(target_ext);

        return Ok(opts.target_path.join(rel_file_path));
    }

    let path_as_rel = match src_path.strip_prefix(&opts.search_path) {
        Ok(path) => path,
        Err(err) => {
            return Err(io::Error::new(io::ErrorKind::InvalidFilename, err.to_string()));
        }
    };

    let mut target_path = opts.target_path.join(&path_as_rel);
    target_path.set_extension(target_ext);

    Ok(target_path)
}

pub fn find_command(command: &Path) -> io::Result<Command> {
    if command.is_relative() {
        let command = env::current_dir()?.join(command);
        Ok(Command::new(command))
    } else {
        Ok(Command::new(command))
    }
}

pub fn is_dirty(target: &Path, source: &Path, opts: &Opts) -> io::Result<bool> {
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
        (Some(compiler_time), Some(src_time)) => {
            Some(SystemTime::max(compiler_time, src_time))
        },
        (Some(time), None) | (None, Some(time)) => {
            Some(time)
        },
        (None, None) => {
            None
        },
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

pub fn apply_compiler_args(case: &TestCase, opts: &Opts, compiler_command: &mut Command) {
    if opts.debug {
        compiler_command.arg("-g");
    }

    for extra_package in &case.script.packages {
        compiler_command.arg("-p").arg(extra_package);
    }

    // ensure the source path is in the lib search dirs
    if let Some(parent_dir) = case.path.parent() {
        compiler_command.arg("-s").arg(parent_dir);
    }

    // expect any extra packages to be present in the same dir as the test source file
    compiler_command.arg("-s").arg(".");
}

pub fn try_run_command(
    command: &mut Command,
    stdout: &mut Vec<u8>,
    stderr: &mut Vec<u8>
) -> io::Result<ExitStatus> {
    let mut output = command.output()?;

    stdout.append(&mut output.stdout);
    stderr.append(&mut output.stderr);

    Ok(output.status)
}

pub fn run_build_command(mut command: Command) -> RunResult<()> {
    let output = command.output()?;

    if !output.status.success() {
        let mut msg = output.status.to_string();

        let compile_stdout = String::from_utf8_lossy(&output.stdout);
        if !compile_stdout.is_empty() {
            msg.push_str("\n");
            msg.push_str(&compile_stdout);
        }

        let compile_stderr = String::from_utf8_lossy(&output.stderr);
        if !compile_stderr.is_empty() {
            msg.push_str("\n");
            msg.push_str(&compile_stderr);
        }

        return Err(RunError::BuildBinaryFailed(msg));
    }

    Ok(())
}