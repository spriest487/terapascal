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

    invoke_clang_format(out_path);

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

fn translate_c<'a>(lib: &'a ir::Library, args: &Args) -> backend_c::ast::Unit<'a> {
    let c_opts = backend_c::Options {
        enable_rtti: args.rtti,
        trace_heap: args.trace_heap,
        trace_rc: args.trace_rc,
        trace_ir: args.trace_ir,
        debug: args.debug,
    };

    backend_c::translate(&lib, c_opts)
}

fn invoke_clang<'a>(
    c_unit: &backend_c::ast::Unit<'a>,
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

    let debug = args.debug || args.debug_codeview;

    if debug {
        // debug: generates a source file next to the output
        let c_file_path = out_path.with_extension("c");

        let mut c_file = File::create(&c_file_path)?;
        write!(c_file, "{c_unit}")?;

        clang_cmd.arg(c_file_path).arg("-g").arg("-O0");

        if args.debug_codeview {
            clang_cmd.arg("-gcodeview");
        }
    } else {
        // release: compile from stdin
        clang_cmd.arg("-").arg("-O2").stdin(Stdio::piped());
    }

    clang_cmd.arg("-o").arg(out_path);

    let mut clang = if debug {
        clang_cmd.spawn()?
    } else {
        let mut clang = clang_cmd.spawn()?;

        let mut clang_in = clang
            .stdin
            .take()
            .ok_or_else(|| io::Error::new(io::ErrorKind::BrokenPipe, "unable to write to stdin"))?;

        write!(clang_in, "{}", c_unit)?;
        clang_in.flush()?;

        clang
    };

    let status = clang.wait()?;

    if !status.success() {
        return Err(io::Error::new(io::ErrorKind::Other, status.to_string()));
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
