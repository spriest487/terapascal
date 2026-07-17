use std::ffi::OsStr;
use std::{env, io};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};
use std::time::SystemTime;
use terapascal_common::IR_LIB_EXT;
use crate::opts::Opts;
use crate::test_case::TestCase;

pub fn compile_lib(
    case: &TestCase,
    opts: &Opts,
    build_stdout: &mut Vec<u8>,
    build_stderr: &mut Vec<u8>,
) -> io::Result<Result<PathBuf, ExitStatus>> {
    let lib_path = target_file_path(&case.path, opts, IR_LIB_EXT)?;
    // eprintln!("module path: {}", module_path.display());

    if is_dirty(&lib_path, &case.path, opts)? {
        let mut build_command = Command::new(&opts.compiler);
        build_command.arg(&case.path);
        build_command.arg("-o").arg(&lib_path);

        apply_compiler_args(&case, opts, &mut build_command);

        let build_status = try_run_command(
            &mut build_command,
            build_stdout,
            build_stderr
        )?;

        if !build_status.success() {
            return Ok(Err(build_status));
        }
    }

    Ok(Ok(lib_path))
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
    if opts.verbose {
        compiler_command.arg("-v");
    }

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
