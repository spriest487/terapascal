mod args;
mod compile_error;
mod reporting;
mod sources;

use crate::args::*;
use crate::compile_error::*;
use crate::reporting::report_err;
use crate::sources::*;
use backend_c as backend_c;
use backend_c::c;
use codespan_reporting::diagnostic::Severity;
use common::read_source_file;
use common::span::*;
use common::BuildOptions;
use frontend::ast;
use frontend::ast::IdentPath;
use frontend::ast::Unit;
use frontend::ast::UnitKind;
use frontend::codegen::IROptions;
use frontend::codegen_ir;
use frontend::parse;
use frontend::pp as pp;
use frontend::tokenize;
use frontend::typ as ty;
use frontend::typ::builtin_ident;
use frontend::typ::SYSTEM_UNIT_NAME;
use frontend::typecheck;
use linked_hash_map::LinkedHashMap;
use pp::PreprocessedUnit;
use std::collections::hash_map::Entry;
use std::collections::hash_map::HashMap;
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
use terapascal_ir as ir;
use terapascal_vm::Interpreter;
use terapascal_vm::InterpreterOpts;
use topological_sort::TopologicalSort;

const IR_LIB_EXT: &str = "lib";

enum CompileOutput {
    Preprocess(Vec<PreprocessedUnit>),
    Parse(Vec<Unit>),
    Typecheck(ty::Module),
    IR(ir::Library),
}

fn preprocess(filename: &PathBuf, opts: BuildOptions) -> Result<PreprocessedUnit, CompileError> {
    let src = read_source_file(filename)
        .map_err(|err| {
            CompileError::ReadSourceFileFailed {
                path: filename.to_path_buf(),
                msg: err.to_string(),
            }
        })?;
    
    let preprocessed = frontend::preprocess(filename, &src, opts)?;
    Ok(preprocessed)
}

fn compile(args: &Args) -> Result<CompileOutput, CompileError> {
    if args.output.is_some() && args.print_stage.is_some() {
        let msg = "output file and print stage arguments are mutually exclusive".to_string();
        return Err(CompileError::InvalidArguments(msg));
    }
    
    let mut opts = BuildOptions::default();
    opts.verbose = args.verbose;
    opts.lang_mode = args.lang_mode;

    for define_sym in &args.define_syms {
        opts.define(define_sym.clone());
    }

    let source_ext = get_extension(&args.file);
    if source_ext.eq_ignore_ascii_case(IR_LIB_EXT) {
        let module = load_module_from_file(&args.file)?;
        return Ok(CompileOutput::IR(module));
    }

    let mut auto_ref_units = Vec::new();

    // map of canonical paths by unit names. each unit must have exactly one source path, to ensure
    // we don't try to load two units with the same name at different locations 
    let mut unit_paths: HashMap<IdentPath, PathBuf> = HashMap::new();

    // auto-add system units if we're going beyond parsing
    let will_typecheck = args.print_stage
        .map(|print_stage| print_stage >= CompileStage::Typecheck)
        .unwrap_or(true);

    if will_typecheck {
        auto_ref_units.push(IdentPath::new(builtin_ident(SYSTEM_UNIT_NAME), []));
    }

    let mut sources = SourceCollection::new(args)?;

    // add builtin units
    for auto_ref_unit in auto_ref_units.into_iter() {
        let unit_filename = PathBuf::from(auto_ref_unit.to_string()).with_extension("pas");
        let unit_path = sources
            .add(&unit_filename, None)?
            .canonicalize()?;

        unit_paths.insert(auto_ref_unit, unit_path);
    }

    // add extra referenced units
    for unit_arg in args.units.iter() {
        let unit_filename = PathBuf::from(unit_arg.clone());
        sources.add(&unit_filename, None)?;
    }

    // add main source unit
    sources.add(&args.file, None)?;

    if opts.verbose {
        println!("Unit source directories:");
        for path in sources.source_dirs() {
            println!("\t{}", path.display());
        }
    }

    // if we just want preprocessor output, no unit refs need to be looked up, just process and
    // print the units provided on the cli
    if let Some(CompileStage::Preprocess) = args.print_stage {
        let mut pp_units = Vec::new();
        while let Some(source_path) = sources.next() {
            let pp_unit = preprocess(&source_path, opts.clone())?;
            pp_units.push(pp_unit);
        }
        return Ok(CompileOutput::Preprocess(pp_units));
    }
    
    // files parsed in the order specified, either by the project unit or on the command line
    let mut parsed_files: LinkedHashMap<PathBuf, Unit> = LinkedHashMap::new();

    // not actually used for sorting, just an easy way to detect circular deps
    let mut dep_sort = TopologicalSort::<IdentPath>::new();

    let mut main_ident = None;

    loop {
        let unit_filename = match sources.next() {
            None => break,
            Some(f) => f,
        };

        let canon_filename = unit_filename
            .canonicalize()
            .map_err(|e| CompileError::ReadSourceFileFailed {
                msg: e.to_string(),
                path: unit_filename.clone(),
            })?;

        let unit = match parsed_files.get(&canon_filename) {
            Some(unit) => {
                return Err(CompileError::DuplicateUnit {
                    unit_ident: unit.ident.clone(),
                    new_path: unit_filename.clone(),
                    existing_path: unit_filename.clone(),
                });
            },

            None => {
                if args.verbose {
                    eprintln!("parsing unit @ `{}`", unit_filename.display());
                }
                
                let pp_unit = preprocess(&canon_filename, opts.clone())?;

                for warning in &pp_unit.warnings {
                    if report_err(warning, Severity::Warning).is_err() {
                        eprintln!("warning: {}", warning);
                    }
                }

                let tokens = tokenize(pp_unit)?;
                let parsed_unit = parse(canon_filename.clone(), tokens)?;

                add_unit_path(&parsed_unit.ident, &unit_filename, &mut unit_paths)?;

                dep_sort.insert(parsed_unit.ident.clone());

                parsed_files.insert(canon_filename.clone(), parsed_unit);
                &parsed_files[&canon_filename]
            }
        };

        let unit_ident = unit.ident.clone();
        
        let is_main_unit = matches!(unit.kind, UnitKind::Program | UnitKind::Library);

        main_ident = match (main_ident, is_main_unit) {
            (None, true) => {
                Some(Some(unit.ident.clone()))
            },

            (None, false) => {
                Some(None)
            },

            (Some(prev_ident), true) => {
                return Err(CompileError::UnexpectedMainUnit {
                    existing_ident: prev_ident,
                    unit_path: canon_filename,
                    unit_kind: unit.kind,
                })
            },

            (existing @ Some(..), false) => {
                existing
            }
        };

        let uses_units: Vec<_> = unit
            .all_decls()
            .filter_map(|(_vis, decl)| match decl {
                ast::UnitDecl::Uses { decl } => Some(decl.clone()),
                _ => None,
            })
            .flat_map(|uses| uses.units)
            .collect();

        for used_unit in uses_units {
            // project units can load other units. without a project unit, each unit to be included
            // in compilation must be included in the units compiler arg
            if !unit_paths.contains_key(&used_unit.ident) {
                if is_main_unit {
                    if args.verbose {
                        println!("unit {} used from {}", used_unit, unit_ident);
                    }

                    let used_unit_path = match used_unit.path {
                        Some(path) => {
                            let filename = PathBuf::from(path);
                            sources.add_used_unit_in_file(&canon_filename, &used_unit.ident, &filename)?
                        }

                        None => {
                            sources.add_used_unit(&canon_filename, &used_unit.ident)?
                        }
                    };

                    add_unit_path(&used_unit.ident, &used_unit_path, &mut unit_paths)?;
                } else {
                    return Err(CompileError::UnitNotLoaded {
                        unit_name: used_unit.ident.clone(),
                    });
                }
            }

            // check for cycles
            dep_sort.add_dependency(used_unit.ident.clone(), unit_ident.clone());
            if dep_sort.peek().is_none() {
                return Err(CompileError::CircularDependency {
                    unit_ident,
                    used_unit: used_unit.ident,
                    span: used_unit.span.clone(),
                });
            }
        }
    }
    
    fn add_unit_path(
        unit_ident: &IdentPath,
        path: &PathBuf,
        unit_paths: &mut HashMap<IdentPath, PathBuf>
    ) -> Result<(), CompileError> {
        let canon_path = path.canonicalize()?;

        match unit_paths.entry(unit_ident.clone()) {
            Entry::Occupied(occupied_ident_filename) => {
                // the same unit can't be loaded from two separate filenames. it might
                // already be in the filename map because we insert the builtin units ahead
                // of time
                if *occupied_ident_filename.get() != canon_path {
                    Err(CompileError::DuplicateUnit {
                        unit_ident: occupied_ident_filename.key().clone(),
                        new_path: path.clone(),
                        existing_path: occupied_ident_filename.get().to_path_buf(),
                    })
                } else {
                    Ok(())
                }
            },

            Entry::Vacant(vacant_ident_filename) => {
                vacant_ident_filename.insert(canon_path.clone());
                
                Ok(())
            }
        }
    }

    let mut compile_units: Vec<_> = parsed_files
        .into_iter()
        .map(|(_path, unit)| unit)
        .collect();
    
    if let Some(CompileStage::Parse) = args.print_stage {
        return Ok(CompileOutput::Parse(compile_units));
    }

    if args.verbose {
        println!("Compilation units:");
        for unit in &compile_units {
            println!("\t{}", unit.ident);
        }
    }

    // reverse the compilation order for typechecking, modules should be processed after all their
    // dependencies
    compile_units.reverse();
    let typed_module = typecheck(&compile_units, args.verbose)?;

    if let Some(CompileStage::Typecheck) = args.print_stage {
        return Ok(CompileOutput::Typecheck(typed_module));
    }

    let ir_opts = IROptions {
        annotate_rc: args.trace_rc,
        debug: args.debug,
    };

    let module = codegen_ir(&typed_module, ir_opts);
    Ok(CompileOutput::IR(module))
}

fn bincode_config() -> bincode::config::Configuration {
    bincode::config::Configuration::default()
}

fn load_module_from_file(path: &Path) -> Result<ir::Library, CompileError> {
    let mut module_bytes = Vec::new();

    File::open(&path)
        .and_then(|mut file| {
            file.read_to_end(&mut module_bytes)
        })
        .map_err(|err| {
            CompileError::ReadSourceFileFailed {
                msg: err.to_string(),
                path: path.to_path_buf(),
            }
        })?;
    
    let module: ir::Library = bincode::serde::decode_from_slice(&module_bytes, bincode_config())
        .map_err(|err| {
            CompileError::ReadSourceFileFailed {
                msg: err.to_string(),
                path: path.to_path_buf(),
            }
        })
        .map(|result| result.0)?;

    Ok(module)
}

fn print_output<F>(out_path: Option<&PathBuf>, f: F) -> Result<(), CompileError>
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

    io_result.map_err(|io_err| CompileError::OutputFailed(out_span, io_err))
}

fn handle_output(output: CompileOutput, args: &Args) -> Result<(), CompileError> {
    match output {
        CompileOutput::Preprocess(units) => print_output(args.output.as_ref(), |dst| {
            for pp_unit in units {
                write!(dst, "{}", pp_unit.source)?;
            }
            Ok(())
        }),

        CompileOutput::Parse(units) => print_output(args.output.as_ref(), |dst| {
            for unit in units {
                write!(dst, "{}", unit)?;
            }

            Ok(())
        }),

        CompileOutput::Typecheck(module) => print_output(args.output.as_ref(), |dst| {
            for unit in &module.units {
                write!(dst, "{}", unit.unit)?;
            }

            Ok(())
        }),

        CompileOutput::IR(lib) => {
            if let Some(CompileStage::EmitIR) = args.print_stage {
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
                        .map_err(|err| CompileError::ClangBuildFailed(err))?;

                    try_clang_format(output_arg);
                    
                    Ok(())
                } else {
                    return Err(CompileError::UnknownOutputFormat(output_ext))
                }
            } else {
                // execute the IR immediately
                let interpret_opts = InterpreterOpts {
                    trace_rc: args.trace_rc,
                    trace_heap: args.trace_heap,
                    trace_ir: args.trace_ir,
                    
                    diag_port: args.diag_port,
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

    if let Err(err) = compile(&args)
        .and_then(|output| handle_output(output, &args)) 
    {
        if let Err(output_err) = report_err(&err, Severity::Error) {
            eprintln!("error: {}", err);
            eprintln!("error reporting output: {}", output_err);
        }

        if print_bt {
            match err {
                CompileError::TokenizeError(err) => {
                    println!("{:?}", err.bt);
                },

                CompileError::ParseError(err) => {
                    println!("{:?}", err.bt);
                },

                _ => {},
            }
        }

        // stop IDE from eating the final line??
        _ = writeln!(io::stderr());
        std::thread::sleep(Duration::from_millis(200));
        
        process::exit(1)
    }
}
