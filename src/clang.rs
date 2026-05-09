use std::path::Path;
use std::process::Stdio;
use std::fs::File;
use std::process::Command;
use std::io;
use std::io::Write;
use crate::error::RunError;
use crate::Args;
use terapascal_ir as ir;
use terapascal_backend_c as backend_c;

pub(crate) fn clang_compile(
    lib: &ir::Library,
    args: &Args,
    out_path: &Path,
) -> Result<(), RunError> {
    let c_unit = translate_c(lib, args);

    invoke_clang(&c_unit, args, out_path)
        .map_err(|err| RunError::ClangBuildFailed(err))?;

    Ok(())
}

pub(crate) fn clang_print(
    lib: &ir::Library,
    args: &Args,
    out_path: &Path,
) -> Result<(), RunError> {
    // output C code
    let c_unit = translate_c(&lib, args);

    crate::print_output(Some(out_path), |dst| {
        write!(dst, "{}", c_unit)
    })?;

    invoke_clang_format(out_path);

    Ok(())
}

fn translate_c<'a>(lib: &'a ir::Library, args: &Args) -> backend_c::c::Unit<'a> {
    let c_opts = backend_c::Options {
        enable_rtti: args.rtti,
        trace_heap: args.trace_heap,
        trace_rc: args.trace_rc,
        trace_ir: args.trace_ir,
        trace_generics: args.trace_generics,
        debug: args.debug,
    };

    backend_c::translate(&lib, c_opts)
}

fn invoke_clang<'a>(
    c_unit: &backend_c::c::Unit<'a>,
    args: &Args,
    out_path: &Path,
) -> io::Result<()> {
    let mut clang_cmd = Command::new("clang");
    clang_cmd
        .stderr(Stdio::inherit())
        .stdout(Stdio::inherit())
        .arg("-Werror")
        .arg("-Wall")
        .arg("-Wextra")
        .arg("-Wno-unused-function")
        .arg("-Wno-unused-parameter")
        .arg("-Wno-unused-variable")
        .arg("-Wno-unused-label")
        .arg("-Wno-address-of-packed-member")
        .arg("-x")
        .arg("c");

    if args.verbose {
        clang_cmd.arg("--verbose");
    }

    clang_cmd.arg("-o").arg(out_path);

    let debug = args.debug || args.debug_codeview;

    let process = if debug {
        // debug: generates a source file next to the output
        let c_file_path = out_path.with_extension("c");

        {
            let mut c_file = File::create(&c_file_path)?;
            write!(c_file, "{c_unit}")?;
            c_file.flush()?;
        }

        invoke_clang_format(&c_file_path);

        clang_cmd.arg(c_file_path);
        clang_cmd.arg("-g");
        clang_cmd.arg("-O0");

        if args.debug_codeview {
            clang_cmd.arg("-gcodeview");
        }

        clang_cmd.spawn()?
    } else {
        // release: compile from stdin
        clang_cmd.arg("-");
        clang_cmd.arg("-O2");
        clang_cmd.stdin(Stdio::piped());

        let mut process = clang_cmd.spawn()?;

        let mut process_in = process
            .stdin
            .take()
            .ok_or_else(|| io::Error::new(io::ErrorKind::BrokenPipe, "unable to write to stdin"))?;

        write!(process_in, "{}", c_unit)?;
        process
    };

    let output = process.wait_with_output()?;

    if !output.status.success() {
        return Err(io::Error::new(io::ErrorKind::Other, output.status.to_string()));
    }

    Ok(())
}

fn invoke_clang_format(path: &Path) {
    let mut clang_cmd = Command::new("clang-format");
    clang_cmd.arg("-i").arg(path);

    let result = clang_cmd.spawn().and_then(|mut child| child.wait());

    match result {
        Ok(exit_status) if exit_status.success() => {},

        Ok(bad_status) => {
            eprintln!("clang-format failed: status {bad_status}")
        },
        Err(err) => {
            eprintln!("clang-format failed: {err}")
        },
    }
}
