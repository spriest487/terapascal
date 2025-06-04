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
use terapascal_common::read_source_file;
use terapascal_common::CompileOpts;
use terapascal_common::SRC_FILE_DEFAULT_EXT;
use terapascal_frontend::ast;
use terapascal_frontend::ast::IdentPath;
use terapascal_frontend::ast::UnitKind;
use terapascal_frontend::codegen::CodegenOpts;
use terapascal_frontend::codegen_ir;
use terapascal_frontend::parse;
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
    pub artifact: BuildArtifact,
    pub log: BuildLog,
}

pub struct ParseOutput {
    pub units: LinkedHashMap<PathBuf, ast::Unit>,
}

pub fn create_source_collection(input: &BuildInput, log: &mut BuildLog) -> BuildResult<SourceCollection> {
    let mut sources = SourceCollection::new(&input.search_dirs, input.compile_opts.verbose)?;

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

pub fn preprocess_project(input: &BuildInput, log: &mut BuildLog) -> BuildResult<Vec<PreprocessedUnit>> {
    let mut sources = create_source_collection(input, log)?;

    let mut pp_units = Vec::new();
    while let Some(source_path) = sources.next() {
        let pp_unit = preprocess(&source_path, input.compile_opts.clone())?;
        pp_units.push(pp_unit);
    }
    
    Ok(pp_units)
}

pub fn parse_units(input: &BuildInput, log: &mut BuildLog) -> BuildResult<ParseOutput> {
    let mut sources = create_source_collection(input, log)?;

    // auto-add system units if we're going beyond parsing
    let include_stdlib = input.output_stage >= BuildStage::Typecheck;
    
    let stdlib_units = [
        IdentPath::from_parts([builtin_ident(SYSTEM_UNIT_NAME)])  
    ];
    
    let mut stdlib_paths = Vec::with_capacity(stdlib_units.len());

    if include_stdlib {
        for stdlib_unit in &stdlib_units {
            let filename = PathBuf::from(stdlib_unit.to_string())
                .with_extension(SRC_FILE_DEFAULT_EXT);

            let unit_path = sources.add(&filename, None, log)?;
            
            stdlib_paths.push(unit_path);
        }
    }

    // files parsed in the order specified, either by the project unit or on the command line
    let mut parsed_units: LinkedHashMap<PathBuf, ast::Unit> = LinkedHashMap::new();

    // not actually used for sorting, just an easy way to detect circular deps
    let mut dep_sort = TopologicalSort::<IdentPath>::new();

    let mut main_ident = None;

    // map of canonical paths by unit names. each unit must have exactly one source path, to ensure
    // we don't try to load two units with the same name at different locations
    let mut used_unit_paths: HashMap<IdentPath, PathBuf> = HashMap::new();

    for (unit_name, path) in stdlib_units.into_iter().zip(stdlib_paths.into_iter()) {
        used_unit_paths.insert(unit_name, path);
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
                if input.compile_opts.verbose {
                    log.trace(format!("parsing unit @ `{}`", unit_filename.display()));
                }

                let pp_unit = preprocess(&unit_filename, input.compile_opts.clone())?;

                for warning in pp_unit.warnings.clone() {
                    log.warn(warning);
                }

                let tokens = tokenize(pp_unit)?;
                let parsed_unit = parse(unit_filename.clone(), tokens)?;

                add_unit_path(&parsed_unit.ident, &unit_filename, &mut used_unit_paths)?;

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

                        None => sources.add_used_unit(&unit_filename, &used_unit.ident, log)?,
                    };

                    add_unit_path(&used_unit.ident, &used_unit_path, &mut used_unit_paths)?;
                } else {
                    return Err(BuildError::UnitNotLoaded {
                        unit_name: used_unit.ident.clone(),
                    });
                }
            }

            // check for cycles
            dep_sort.add_dependency(used_unit.ident.clone(), unit_ident.clone());
            if dep_sort.peek().is_none() {
                return Err(BuildError::CircularDependency {
                    unit_ident,
                    used_unit: used_unit.ident,
                    span: used_unit.span.clone(),
                });
            }
        }
    }

    fn add_unit_path(
        unit_ident: &IdentPath,
        unit_path: &PathBuf,
        unit_paths: &mut HashMap<IdentPath, PathBuf>,
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
                vacant_ident_filename.insert(unit_path.clone());

                Ok(())
            },
        }
    }

    if input.compile_opts.verbose {
        log.trace("Compilation units:");
        for (unit_path, unit) in &parsed_units {
            log.trace(format!("\t{} in '{}'", unit.ident, unit_path.display()));
        }
    }
    
    Ok(ParseOutput {
        units: parsed_units,
    })
}

pub fn build(input: BuildInput) -> Result<BuildOutput, BuildError> {
    let mut log = BuildLog::new();

    // if we just want preprocessor output, no unit refs need to be looked up, just process and
    // print the units provided on the cli in that order
    if input.output_stage == BuildStage::Preprocess {
        let units = preprocess_project(&input, &mut log)?;
        return Ok(BuildOutput {
            artifact: BuildArtifact::PreprocessedText(units),
            log,
        });
    }

    let parse_output = parse_units(&input, &mut log)?;

    if input.output_stage == BuildStage::Parse {
        return Ok(BuildOutput {
            artifact: BuildArtifact::ParsedUnits(parse_output),
            log,
        });
    }

    // reverse the compilation order for typechecking, modules should be processed after all their
    // dependencies
    let typed_module = typecheck(parse_output.units.iter(), input.compile_opts.verbose, &mut log)?;

    if input.output_stage == BuildStage::Typecheck {
        return Ok(BuildOutput {
            artifact: BuildArtifact::TypedModule(typed_module),
            log,
        });
    }

    let library = codegen_ir(&typed_module, input.codegen_opts);

    Ok(BuildOutput { 
        artifact: BuildArtifact::Library(library),
        log,
    })
}

pub fn bincode_config() -> bincode::config::Configuration {
    bincode::config::Configuration::default()
}

fn preprocess(filename: &PathBuf, opts: CompileOpts) -> Result<PreprocessedUnit, BuildError> {
    let src = read_source_file(filename).map_err(|err| BuildError::ReadSourceFileFailed {
        path: filename.to_path_buf(),
        msg: err.to_string(),
    })?;

    let preprocessed = terapascal_frontend::preprocess(filename, &src, opts)?;
    Ok(preprocessed)
}
