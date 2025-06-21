pub mod error;
pub mod sources;

use crate::error::BuildError;
use crate::error::BuildResult;
use crate::sources::SourceCollection;
use linked_hash_map::LinkedHashMap;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::path::PathBuf;
use terapascal_backend_c::ir;
use terapascal_common::build_log::BuildLog;
use terapascal_common::fs::Filesystem;
use terapascal_common::CompileOpts;
use terapascal_common::{TracedError, SRC_FILE_DEFAULT_EXT};
use terapascal_frontend::ast;
use terapascal_frontend::ast::IdentPath;
use terapascal_frontend::ast::UnitKind;
use terapascal_frontend::codegen::CodegenOpts;
use terapascal_frontend::codegen_ir;
use terapascal_frontend::parse;
use terapascal_frontend::parse::ParseError;
use terapascal_frontend::pp::PreprocessedUnit;
use terapascal_frontend::tokenize;
use terapascal_frontend::typ;
use terapascal_frontend::typ::builtin_ident;
use terapascal_frontend::typ::SYSTEM_UNIT_NAME;
use terapascal_frontend::typecheck;
use topological_sort::TopologicalSort;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Hash)]
pub enum BuildStage {
    Preprocess,
    Parse,
    Typecheck,
    Codegen,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuildInput {
    pub units: Vec<PathBuf>,
    pub search_dirs: Vec<PathBuf>,

    pub output_stage: BuildStage,

    pub compile_opts: CompileOpts,
    pub codegen_opts: CodegenOpts,
}

pub enum BuildArtifact {
    PreprocessedText(Vec<PreprocessedUnit>),
    ParsedUnits(ParseOutput),
    TypedModule(typ::Module),
    Library(ir::Library),
}

pub struct BuildOutput {
    pub artifact: BuildResult<BuildArtifact>,
    pub log: BuildLog,
}

pub struct ParseOutput {
    pub units: LinkedHashMap<PathBuf, ast::Unit>,
}

pub fn create_source_collection<'fs, Fs: Filesystem>(fs: &'fs Fs, input: &BuildInput, log: &mut BuildLog) -> BuildResult<SourceCollection<'fs, Fs>> {
    let mut sources = SourceCollection::new(fs, &input.search_dirs, input.compile_opts.verbose)?;

    // add extra referenced units
    for unit_arg in input.units.iter() {
        let unit_filename = PathBuf::from(unit_arg.clone());
        sources.add(&unit_filename, None, log)?;
    }

    if input.compile_opts.verbose {
        log.trace("Unit source directories:");
        for path in sources.source_dirs() {
            log.trace(format!("\t{}", path.display()));
        }
    }
    
    Ok(sources)
}

pub fn preprocess_project(fs: &impl Filesystem, input: &BuildInput, log: &mut BuildLog) -> BuildResult<Vec<PreprocessedUnit>> {
    let mut sources = create_source_collection(fs, input, log)?;

    let mut pp_units = Vec::new();
    while let Some(source_path) = sources.next() {
        let pp_unit = preprocess(fs, &source_path, input.compile_opts.clone())?;
        pp_units.push(pp_unit);
    }
    
    Ok(pp_units)
}

pub fn parse_units(fs: &impl Filesystem, input: &BuildInput, log: &mut BuildLog) -> BuildResult<ParseOutput> {
    let verbose = input.compile_opts.verbose;

    let mut sources = create_source_collection(fs, input, log)?;

    // auto-add system units if we're going beyond parsing
    let include_system = input.output_stage >= BuildStage::Typecheck;
    
    let system_units = [
        IdentPath::from_parts([builtin_ident(SYSTEM_UNIT_NAME)])  
    ];

    let mut system_paths = Vec::with_capacity(system_units.len());

    if include_system {
        for stdlib_unit in &system_units {
            let filename = PathBuf::from(stdlib_unit.to_string())
                .with_extension(SRC_FILE_DEFAULT_EXT);

            let unit_path = sources.add(&filename, None, log)?;
            
            system_paths.push(unit_path);
        }
    }

    // files parsed in the order specified, either by the project unit or on the command line
    let mut parsed_units: LinkedHashMap<PathBuf, ast::Unit> = LinkedHashMap::new();

    let mut dep_sort = TopologicalSort::<IdentPath>::new();

    let mut main_ident = None;

    // map of canonical paths by unit names. each unit must have exactly one source path, to ensure
    // we don't try to load two units with the same name at different locations
    let mut used_unit_paths: HashMap<IdentPath, PathBuf> = HashMap::new();

    for (unit_name, path) in system_units.iter().zip(system_paths.into_iter()) {
        used_unit_paths.insert(unit_name.clone(), path);
    }

    loop {
        let unit_filename = match sources.next() {
            None => break,
            Some(f) => f,
        };

        let unit = match parsed_units.get(&unit_filename) {
            Some(unit) => {
                return Err(BuildError::DuplicateUnit {
                    unit_ident: unit.ident.clone(),
                    new_path: unit_filename.clone(),
                    existing_path: unit_filename.clone(),
                });
            },

            None => {
                if verbose {
                    log.trace(format!("parsing unit @ `{}`", unit_filename.display()));
                }

                let pp_unit = preprocess(fs, &unit_filename, input.compile_opts.clone())?;

                for warning in pp_unit.warnings.clone() {
                    log.warn(warning);
                }

                let tokens = tokenize(pp_unit)?;

                // if the unit is parsed to completion with errors, add the errors to the log
                // and continue with the partially parsed unit
                let parsed_unit = match parse(unit_filename.clone(), tokens) {
                    Ok(unit) => unit,
                    Err(err) => match err.err {
                        ParseError::AggregateUnit(err) => {
                            let (unit, errors) = err.unwrap();
                            for error in errors {
                                log.error(error);
                            }
                            
                            unit
                        },

                        other => {
                            return Err(BuildError::from(TracedError {
                                err: other,
                                bt: err.bt
                            }));
                        },
                    }
                };

                add_unit_path(&parsed_unit.ident, &unit_filename, &mut used_unit_paths, verbose, log)?;

                dep_sort.insert(parsed_unit.ident.clone());

                parsed_units.insert(unit_filename.clone(), parsed_unit);
                &parsed_units[&unit_filename]
            },
        };

        let unit_ident = unit.ident.clone();

        let is_main_unit = matches!(unit.kind, UnitKind::Program | UnitKind::Library);

        main_ident = match (main_ident, is_main_unit) {
            (None, true) => Some(Some(unit.ident.clone())),

            (None, false) => Some(None),

            (Some(prev_ident), true) => {
                return Err(BuildError::UnexpectedMainUnit {
                    existing_ident: prev_ident,
                    unit_path: unit_filename,
                    unit_kind: unit.kind,
                });
            },

            (existing @ Some(..), false) => existing,
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
            if !used_unit_paths.contains_key(&used_unit.ident) {
                if is_main_unit {
                    if input.compile_opts.verbose {
                        log.trace(format!("unit {} used from {}", used_unit, unit_ident));
                    }

                    let used_unit_path = match used_unit.path {
                        Some(path) => {
                            let filename = PathBuf::from(path);
                            sources.add_used_unit_in_file(
                                &unit_filename,
                                &used_unit.ident,
                                &filename,
                                log,
                            )?
                        },

                        None => {
                            sources.add_used_unit(&unit_filename, &used_unit.ident, log)?
                        },
                    };

                    add_unit_path(&used_unit.ident, &used_unit_path, &mut used_unit_paths, verbose, log)?;
                } else {
                    return Err(BuildError::UnitNotLoaded {
                        unit_name: used_unit.ident.clone(),
                    });
                }
            }

            add_unit_dep(&unit_ident, &used_unit.ident, &mut dep_sort, log, verbose)?;
        }
    }
    
    let mut sorted_unit_names: Vec<_> = dep_sort.collect();
    sorted_unit_names.reverse();

    let mut sorted_units = LinkedHashMap::new();    
    for unit_name in sorted_unit_names {
        let unit_path = &used_unit_paths[&unit_name];
        let unit = parsed_units.remove(unit_path).unwrap();
        sorted_units.insert(unit_path.clone(), unit);
    }

    drop(parsed_units);

    if verbose {
        log.trace("Compilation units:");
        for (unit_path, unit) in &sorted_units {
            log.trace(format!("\t{} in '{}'", unit.ident, unit_path.display()));
        }
    }
    
    Ok(ParseOutput {
        units: sorted_units,
    })
}

fn add_unit_dep(
    unit_ident: &IdentPath,
    used_unit: &IdentPath,
    dep_sort: &mut TopologicalSort<IdentPath>,
    log: &mut BuildLog,
    verbose: bool,
) -> BuildResult<()> {
    if verbose {
        log.trace(format!("Unit dependency: {unit_ident} -> {used_unit}"));
    }
    
    dep_sort.add_dependency(used_unit.clone(), unit_ident.clone());

    // check for cycles
    if dep_sort.peek().is_none() {
        return Err(BuildError::CircularDependency {
            unit_ident: unit_ident.clone(),
            used_unit: used_unit.clone(),
            span: used_unit.path_span(),
        });
    }

    Ok(())
}

fn add_unit_path(
    unit_ident: &IdentPath,
    unit_path: &PathBuf,
    unit_paths: &mut HashMap<IdentPath, PathBuf>,
    verbose: bool,
    log: &mut BuildLog,
) -> Result<(), BuildError> {
    match unit_paths.entry(unit_ident.clone()) {
        Entry::Occupied(occupied_ident_filename) => {
            // the same unit can't be loaded from two separate filenames. it might
            // already be in the filename map because we insert the builtin units ahead
            // of time
            if *occupied_ident_filename.get() != *unit_path {
                Err(BuildError::DuplicateUnit {
                    unit_ident: occupied_ident_filename.key().clone(),
                    new_path: unit_path.clone(),
                    existing_path: occupied_ident_filename.get().to_path_buf(),
                })
            } else {
                Ok(())
            }
        },

        Entry::Vacant(vacant_ident_filename) => {
            if verbose {
                log.trace(format!("{}: adding filename {}", vacant_ident_filename.key(), unit_path.display()))
            }
            
            vacant_ident_filename.insert(unit_path.clone());

            Ok(())
        },
    }
}

pub fn build(fs: &impl Filesystem, input: BuildInput) -> BuildOutput {
    let mut log = BuildLog::new();
    
    let artifact = build_with_log(fs, input, &mut log);

    BuildOutput {
        artifact,
        log,
    }
}

fn build_with_log(fs: &impl Filesystem, input: BuildInput, log: &mut BuildLog) -> BuildResult<BuildArtifact> {
    // if we just want preprocessor output, no unit refs need to be looked up, just process and
    // print the units provided on the cli in that order
    if input.output_stage == BuildStage::Preprocess {
        let units = preprocess_project(fs, &input, log)?;
        return Ok(BuildArtifact::PreprocessedText(units));
    }

    let parse_output = parse_units(fs, &input, log)?;

    if input.output_stage == BuildStage::Parse {
        return Ok(BuildArtifact::ParsedUnits(parse_output));
    }

    // reverse the compilation order for typechecking, modules should be processed after all their
    // dependencies
    let typed_module = typecheck(parse_output.units.iter(), input.compile_opts.verbose, log)?;

    if input.output_stage == BuildStage::Typecheck {
        return Ok(BuildArtifact::TypedModule(typed_module));
    }

    if log.has_errors() {
        return Err(BuildError::CompletedWithErrors);
    }

    let library = codegen_ir(&typed_module, input.codegen_opts);

    Ok(BuildArtifact::Library(library))
}

pub fn bincode_config() -> bincode::config::Configuration {
    bincode::config::Configuration::default()
}

fn preprocess(fs: &impl Filesystem, filename: &PathBuf, opts: CompileOpts) -> Result<PreprocessedUnit, BuildError> {
    let src = fs.read_source(filename).map_err(|err| BuildError::ReadSourceFileFailed {
        path: filename.to_path_buf(),
        msg: err.to_string(),
    })?;

    let preprocessed = terapascal_frontend::preprocess(fs, filename, &src, opts)?;
    Ok(preprocessed)
}
