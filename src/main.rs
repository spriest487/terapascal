mod args;
mod error;

#[cfg(feature = "backend-c")]
mod clang;

#[cfg(feature = "backend-cil")]
mod dotnet;

use crate::args::*;
use crate::error::*;
use std::env;
use std::ffi::OsString;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io;
use std::io::Write as _;
use std::path::Path;
use std::process;
use std::time::Duration;
use structopt::StructOpt;
use terapascal_build::build;
use terapascal_build::BuildArtifact;
use terapascal_build::BuildInput;
use terapascal_build::BuildOutput;
use terapascal_build::BuildStage;
use terapascal_build::LibraryLoader;
use terapascal_common::build_log::BuildLog;
use terapascal_common::build_log::BuildLogEntry;
use terapascal_common::fs::DefaultFilesystem;
use terapascal_common::reporting::report_err;
use terapascal_common::span::*;
use terapascal_common::CompileOpts;
use terapascal_common::DiagnosticOutput;
use terapascal_common::CIL_LIB_EXT;
use terapascal_common::IR_LIB_EXT;
use terapascal_frontend::codegen::CodegenOpts;
use terapascal_ir as ir;
use terapascal_ir::IRFormatter;
use terapascal_ir::RawFormatter;
use terapascal_vm::result::ExecResult;
use terapascal_vm::ExecOpts;
use terapascal_vm::Vm;

#[cfg(feature = "backend-c")]
use clang::clang_compile;

#[cfg(feature = "backend-c")]
use clang::clang_print;

#[cfg(feature = "backend-cil")]
use dotnet::dotnet_build;

fn compile(args: Args) -> Result<(), RunError> {
    if args.output.is_some() && args.dump_stage.is_some() {
        let msg = "output file and print stage arguments are mutually exclusive".to_string();
        return Err(RunError::InvalidArguments(msg));
    }

    let mut compile_opts = CompileOpts::default();
    compile_opts.verbose = args.verbose;
    compile_opts.lang_mode = args.lang_mode;
    compile_opts.allow_unsafe = args.allow_unsafe;
    compile_opts.no_system = args.no_system;

    if args.rtti {
        compile_opts.define("RTTI");
    }

    if args.debug {
        compile_opts.define("DEBUG");
    }

    compile_opts.define(env::consts::OS.to_ascii_uppercase());

    let codegen_opts = CodegenOpts {
        debug: args.debug,
        rtti: args.rtti,
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

        let mut lib_loader = LibraryLoader::new(args.search_dirs.clone(), &mut log);

        let artifact = lib_loader.load_lib_file(&args.file)
            .map(BuildArtifact::Library);

        return handle_output(BuildOutput {
            artifact,
            log,
        }, &args);
    }
    
    let input = BuildInput {
        compile_opts,
        codegen_opts,
        project_version: args.project_version,
        project_name: args.project_name.clone(),
        search_dirs: args.search_dirs.clone(),
        output_stage: args.dump_stage.unwrap_or(BuildStage::Codegen),
        source_path: args.file.clone(),
        package_names: args.packages.clone(),
        no_system: args.no_system,
    };
    
    let output = build(&DefaultFilesystem, input);
    
    handle_output(output, &args)
}

fn write_output<OutFn>(out_path: Option<&Path>, f: OutFn) -> Result<(), RunError>
where
    OutFn: FnOnce(&mut dyn io::Write) -> io::Result<()>,
{
    let out_span;

    let io_result = match out_path {
        Some(out_path) => {
            let create_dirs = match out_path.parent() {
                Some(parent) => fs::create_dir_all(parent),
                None => Ok(()),
            };

            out_span = Span::zero(out_path);

            create_dirs
                .and_then(|_| File::create(out_path))
                .and_then(|mut file| {
                    f(&mut file)
                })
        },

        None => {
            out_span = Span::zero("stdout");

            f(&mut io::stdout().lock())
        },
    };

    io_result.map_err(|io_err| RunError::OutputFailed(out_span, io_err))
}

fn print_output<OutFn>(out_path: Option<&Path>, f: OutFn) -> Result<(), RunError>
where
    OutFn: FnOnce(&mut dyn fmt::Write) -> fmt::Result,
{
    write_output(out_path, |dst| {
        let mut buf = String::new();

        f(&mut buf).map_err(|err| {
            io::Error::new(io::ErrorKind::InvalidData, err)
        })?;

        write!(dst, "{buf}")
    })
}

fn handle_output(output: BuildOutput, args: &Args) -> Result<(), RunError> {
    for log_entry in &output.log.entries {
        match log_entry {
            BuildLogEntry::Trace(trace) => {
                if args.verbose {
                    println!("[trace] {}", trace);
                }
            }

            BuildLogEntry::Diagnostic(warning) => {
                if report_err(warning.as_ref()).is_err() {
                    eprintln!("[{}] {}", warning.severity(), warning.main());
                }
            }
        }
    }
    
    match output.artifact? {
        BuildArtifact::PreprocessedText(units) => print_output(args.output_path(), |dst| {
            for pp_unit in units {
                writeln!(dst, "{}:", pp_unit.filename.display())?;
                write!(dst, "{}", pp_unit.source)?;
            }
            Ok(())
        }),

        BuildArtifact::ParsedUnits(parse_output) => print_output(args.output_path(), |dst| {
            for (path, unit) in parse_output.units {
                writeln!(dst, "{}:", path.display())?;
                write!(dst, "{}", unit)?;
            }

            Ok(())
        }),

        BuildArtifact::TypedModule(module) => {
            print_output(
                args.output_path(), 
                |dst| {
                    for unit in &module.units {
                        writeln!(dst, "{}:", unit.path.display())?;
                        write!(dst, "{}", unit.unit)?;
                    }

                    Ok(())
                }
            )
        },

        BuildArtifact::Library(libs) => {
            if let Some(BuildStage::Codegen) = args.dump_stage {
                return print_output(args.output_path(), |dst: &mut dyn fmt::Write| {
                    let metadata = libs.to_metadata_builder();
                    libs.main.format(dst, &metadata)
                });
            }

            if let Some(out_path) = args.output_path() {
                let output_ext = out_path
                    .extension()
                    .map(|ext| ext.to_os_string())
                    .unwrap_or_else(OsString::new);

                if output_ext.eq_ignore_ascii_case(IR_LIB_EXT) {
                    // the IR object is the output
                    let module_bytes = ir::encode_lib(&libs.main)?;

                    write_output(Some(out_path), |dst| {
                        dst.write_all(&module_bytes)
                    })
                } else {
                    let merged_lib = libs.merge();

                    if output_ext.eq_ignore_ascii_case("c") {
                        clang_print(&merged_lib, args, out_path)?;
                        Ok(())
                    } else if output_ext.eq_ignore_ascii_case(env::consts::EXE_EXTENSION) {
                        clang_compile(&merged_lib, args, out_path)?;
                        Ok(())
                    } else if output_ext.eq_ignore_ascii_case(CIL_LIB_EXT) && args.arch == TargetArch::Cil {
                        dotnet_build(&merged_lib, &args, out_path)?;

                        Ok(())
                    } else {
                        return Err(RunError::UnknownOutputExt(output_ext))
                    }
                }
            } else {
                let err_formatter: Box<dyn IRFormatter> = if args.debug {
                    Box::new(libs.to_metadata_builder())
                } else {
                    Box::new(RawFormatter)
                };

                exec_vm(args, libs.iter()).map_err(|err| err.map_types(|ty| {
                    ty.to_pretty_string(err_formatter.as_ref())
                }))?;

                Ok(())
            }
        }
    }
}

fn exec_vm<'a>(args: &Args, libs: impl IntoIterator<Item=&'a ir::Library>) -> ExecResult<()> {
    // execute the IR immediately
    let exec_opts = ExecOpts {
        trace_rc: args.trace_rc,
        trace_heap: args.trace_heap,
        trace_ir: args.trace_ir,
        trace_generics: args.trace_generics,
        
        leak_check: args.debug,

        diag_port: args.diag_port,

        verbose: args.verbose,
    };

    let mut vm = Vm::new(exec_opts)?;
    for lib in libs {
        vm.load_lib(lib)?;
    }
    vm.shutdown()?;

    Ok(())
}

#[cfg(not(feature = "backend-c"))]
pub(crate) fn clang_compile(_module: &ir::Library, _args: &Args, _out_path: &Path) -> Result<(), RunError> {
    let msg = "C backend is unavailable".to_string();

    Err(RunError::InvalidArguments(msg))
}

#[cfg(not(feature = "backend-c"))]
pub(crate) fn clang_print(_module: &ir::Library, _args: &Args, _out_path: &Path) -> Result<(), RunError> {
    let msg = "C backend is unavailable".to_string();

    Err(RunError::InvalidArguments(msg))
}

#[cfg(not(feature = "backend-cil"))]
pub(crate) fn dotnet_build(_module: &ir::Library, _args: &Args, _out_path: &Path) -> Result<(), RunError> {
    let msg = "CIL backend is unavailable".to_string();

    Err(RunError::InvalidArguments(msg))
}

fn get_extension(path: &Path) -> String {
    path.extension()
        .map(|ext| ext.to_string_lossy().into_owned())
        .unwrap_or_else(String::new)
} 

fn main() {
    let args: Args = Args::from_args();

    let print_bt = args.backtrace;

    if let Err(err) = compile(args) {
        if let Err(output_err) = report_err(&err) {
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
