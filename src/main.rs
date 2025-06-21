mod args;
mod compile_error;
mod reporting;

use crate::args::*;
use crate::compile_error::*;
use crate::reporting::report_err;
use codespan_reporting::diagnostic::Severity as ReportSeverity;
use std::env;
use std::ffi::OsStr;
use std::fs;
use std::fs::File;
use std::io;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process;
use std::process::Command;
use std::process::Stdio;
use std::time::Duration;
use structopt::StructOpt;
use terapascal_backend_c as backend_c;
use terapascal_backend_c::c;
use terapascal_build::bincode_config;
use terapascal_build::build;
use terapascal_build::error::{BuildError, BuildResult};
use terapascal_build::BuildArtifact;
use terapascal_build::BuildInput;
use terapascal_build::BuildOutput;
use terapascal_build::BuildStage;
use terapascal_common::build_log::BuildLog;
use terapascal_common::build_log::BuildLogEntry;
use terapascal_common::span::*;
use terapascal_common::{CompileOpts, Severity};
use terapascal_common::DiagnosticOutput;
use terapascal_common::fs::DefaultFilesystem;
use terapascal_common::IR_LIB_EXT;
use terapascal_frontend::codegen::CodegenOpts;
use terapascal_ir as ir;
use terapascal_vm::Interpreter;
use terapascal_vm::InterpreterOpts;

fn compile(args: &Args) -> Result<(), RunError> {
    if args.output.is_some() && args.print_stage.is_some() {
        let msg = "output file and print stage arguments are mutually exclusive".to_string();
        return Err(RunError::InvalidArguments(msg));
    }

    let mut compile_opts = CompileOpts::default();
    compile_opts.verbose = args.verbose;
    compile_opts.lang_mode = args.lang_mode;

    let codegen_opts = CodegenOpts {
        annotate_rc: args.trace_rc,
        debug: args.debug,
    };

    for define_sym in &args.define_syms {
        compile_opts.define(define_sym.clone());
    }

    let source_ext = get_extension(&args.file);
    if source_ext.eq_ignore_ascii_case(IR_LIB_EXT) {
        let mut log = BuildLog::new();
        if compile_opts.verbose {
            log.trace(format!("loading existing module: {}", args.file.display()));
        }

        let artifact = load_lib(&args.file)
            .map(BuildArtifact::Library);
        
        return handle_output(BuildOutput {
            artifact,
            log,
        }, args);
    }
    
    let mut units = args.units.clone();

    // add main source unit
    units.push(args.file.clone());
    
    let input = BuildInput {
        compile_opts,
        codegen_opts,
        search_dirs: args.search_dirs.clone(),
        output_stage: args.print_stage.unwrap_or(BuildStage::Codegen),
        units,
    };
    
    let output = build(&DefaultFilesystem, input);
    
    handle_output(output, args)
}

fn load_lib(path: &Path) -> BuildResult<ir::Library> {
    let mut module_bytes = Vec::new();

    File::open(&path)
        .and_then(|mut file| {
            file.read_to_end(&mut module_bytes)
        })
        .map_err(|err| {
            BuildError::ReadSourceFileFailed {
                msg: err.to_string(),
                path: path.to_path_buf(),
            }
        })?;

    let module: ir::Library = bincode::serde::decode_from_slice(&module_bytes, bincode_config())
        .map_err(|err| {
            BuildError::ReadSourceFileFailed {
                msg: err.to_string(),
                path: path.to_path_buf(),
            }
        })
        .map(|result| result.0)?;

    Ok(module)
}

fn print_output<F>(out_path: Option<&PathBuf>, f: F) -> Result<(), RunError>
where
    F: FnOnce(&mut dyn io::Write) -> io::Result<()>,
{
    let out_span;

    let io_result = match out_path {
        Some(out_path) => {
            let create_dirs = match out_path.parent() {
                Some(parent) => fs::create_dir_all(parent),
                None => Ok(()),
            };

            out_span = Span::zero(out_path.clone());

            create_dirs
                .and_then(|_| File::create(out_path))
                .and_then(|mut file| f(&mut file))
        },

        None => {
            let stdout = io::stdout();
            let mut stdout_lock = stdout.lock();

            out_span = Span::zero("stdout");

            f(&mut stdout_lock)
        },
    };

    io_result.map_err(|io_err| RunError::OutputFailed(out_span, io_err))
}

fn handle_output(output: BuildOutput, args: &Args) -> Result<(), RunError> {
    for log_entry in &output.log.entries {
        match log_entry {
            BuildLogEntry::Trace(trace) => {
                println!("{}", trace);
            }

            BuildLogEntry::Warn(warning) => {
                if report_err(warning.as_ref(), ReportSeverity::Warning).is_err() {
                    eprintln!("warning: {}", warning.main(Severity::Warning));
                }
            }

            BuildLogEntry::Error(warning) => {
                if report_err(warning.as_ref(), ReportSeverity::Error).is_err() {
                    eprintln!("warning: {}", warning.main(Severity::Error));
                }
            }
        }
    }
    
    match output.artifact? {
        BuildArtifact::PreprocessedText(units) => print_output(args.output.as_ref(), |dst| {
            for pp_unit in units {
                writeln!(dst, "{}:", pp_unit.filename.display())?;
                write!(dst, "{}", pp_unit.source)?;
            }
            Ok(())
        }),

        BuildArtifact::ParsedUnits(parse_output) => print_output(args.output.as_ref(), |dst| {
            for (path, unit) in parse_output.units {
                writeln!(dst, "{}:", path.display())?;
                write!(dst, "{}", unit)?;
            }

            Ok(())
        }),

        BuildArtifact::TypedModule(module) => print_output(args.output.as_ref(), |dst| {
            for unit in &module.units {
                writeln!(dst, "{}:", unit.path.display())?;
                write!(dst, "{}", unit.unit)?;
            }

            Ok(())
        }),

        BuildArtifact::Library(lib) => {
            if let Some(BuildStage::Codegen) = args.print_stage {
                return print_output(args.output.as_ref(), |dst| write!(dst, "{}", lib));
            }
            
            if let Some(output_arg) = args.output.as_ref() {
                let output_ext = args.output.as_ref()
                    .map(|out_path| get_extension(out_path))
                    .unwrap_or_else(String::new);

                if output_ext.eq_ignore_ascii_case("c") {
                    // output C code
                    let c_unit = translate_c(&lib, args);

                    print_output(Some(output_arg), |dst| {
                        write!(dst, "{}", c_unit)
                    })?;

                    try_clang_format(output_arg);
                    
                    Ok(())
                } else if output_ext.eq_ignore_ascii_case(IR_LIB_EXT) {
                    // the IR object is the output
                    let module_bytes = bincode::serde::encode_to_vec(&lib, bincode_config())?;

                    print_output(args.output.as_ref(), |dst| {
                        dst.write_all(&module_bytes)
                    })
                } else if output_ext.eq_ignore_ascii_case(env::consts::EXE_EXTENSION) {
                    clang_compile(&lib, args, output_arg.as_os_str())
                        .map_err(|err| RunError::ClangBuildFailed(err))?;

                    try_clang_format(output_arg);
                    
                    Ok(())
                } else {
                    return Err(RunError::UnknownOutputFormat(output_ext))
                }
            } else {
                // execute the IR immediately
                let interpret_opts = InterpreterOpts {
                    trace_rc: args.trace_rc,
                    trace_heap: args.trace_heap,
                    trace_ir: args.trace_ir,
                    
                    diag_port: args.diag_port,
                    
                    verbose: args.verbose,
                };

                let mut interpreter = Interpreter::new(interpret_opts);
                interpreter.load_lib(&lib)?;
                interpreter.shutdown()?;

                Ok(())
            }
        }
    }
}

fn translate_c(module: &ir::Library, args: &Args) -> c::Unit {
    let c_opts = backend_c::Options {
        trace_heap: args.trace_heap,
        trace_rc: args.trace_rc,
        trace_ir: args.trace_ir,
        debug: args.debug,
    };

    backend_c::translate(&module, c_opts)
}

fn clang_compile(module: &ir::Library, args: &Args, out_path: &OsStr) -> io::Result<()> {
    let c_unit = translate_c(module, args);
    
    let mut clang_cmd = Command::new("clang");
    clang_cmd
        .arg("-Werror")
        .arg("-Wall")
        .arg("-Wextra")
        .arg("-Wno-unused-function")
        .arg("-Wno-unused-parameter")
        .arg("-Wno-unused-variable")
        .arg("-Wno-unused-label")
        .arg("-x").arg("c")
        .arg("-o").arg(out_path);
    
    let debug = args.debug || args.debug_codeview;

    let mut clang = if debug {
        // debug: generate a source file next to the output
        let c_file_path = PathBuf::from(out_path).with_extension("c");
        
        let mut c_file = File::create(&c_file_path)?; 
        write!(c_file, "{c_unit}")?;

        clang_cmd.arg(c_file_path)
            .arg("-g")
            .arg("-O0");

        if args.debug_codeview {
            clang_cmd.arg("-gcodeview");
        }

        clang_cmd.spawn()?
    } else {
        // release: compile from stdin
        clang_cmd.arg("-")
            .arg("-O2")
            .stdin(Stdio::piped());

        let mut clang = clang_cmd.spawn()?;

        let mut clang_in = clang.stdin
            .take()
            .ok_or_else(|| io::Error::new(io::ErrorKind::BrokenPipe, "unable to write to stdin"))?;

        write!(clang_in, "{}", c_unit)?;
        drop(clang_in);
        
        clang
    };

    let status = clang.wait()?;

    if !status.success() {
        return Err(io::Error::new(io::ErrorKind::Other, status.to_string()));
    }
    
    Ok(())
}

fn try_clang_format(path: &Path) {
    let mut clang_cmd = Command::new("clang-format");
    clang_cmd.arg("-i").arg(path);
    
    let result = clang_cmd
        .spawn()
        .and_then(|mut child| child.wait());
    
    match result {
        Ok(exit_status) if exit_status.success() => {}

        Ok(bad_status) => {
            eprintln!("clang-format failed: status {bad_status}")
        }
        Err(err) => {
            eprintln!("clang-format failed: {err}")
        }
    }
}

fn get_extension(path: &Path) -> String {
    path.extension()
        .map(|ext| ext.to_string_lossy().into_owned())
        .unwrap_or_else(String::new)
} 

fn main() {
    let args: Args = Args::from_args();

    let print_bt = args.backtrace;

    if let Err(err) = compile(&args) {
        if let Err(output_err) = report_err(&err, ReportSeverity::Error) {
            eprintln!("error: {}", err);
            eprintln!("error reporting output: {}", output_err);
        }

        if print_bt {
            if let Some(bt) = err.backtrace() {
                println!("{:?}", bt);
            }
        }

        // stop IDE from eating the final line??
        _ = writeln!(io::stderr());
        std::thread::sleep(Duration::from_millis(200));
        
        process::exit(1)
    }
}
