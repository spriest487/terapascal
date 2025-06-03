pub mod error;
pub mod sources;

use crate::error::BuildError;
use crate::sources::SourceCollection;
use linked_hash_map::LinkedHashMap;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::path::PathBuf;
use terapascal_backend_c::ir;
use terapascal_common::CompileOpts;
use terapascal_common::DiagnosticOutput;
use terapascal_common::SRC_FILE_DEFAULT_EXT;
use terapascal_common::read_source_file;
use terapascal_frontend::ast;
use terapascal_frontend::ast::IdentPath;
use terapascal_frontend::ast::UnitKind;
use terapascal_frontend::codegen::CodegenOpts;
use terapascal_frontend::codegen_ir;
use terapascal_frontend::parse;
use terapascal_frontend::pp::PreprocessedUnit;
use terapascal_frontend::tokenize;
use terapascal_frontend::typ;
use terapascal_frontend::typ::SYSTEM_UNIT_NAME;
use terapascal_frontend::typ::builtin_ident;
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
    ParsedUnit(Vec<ast::Unit>),
    TypedModule(typ::Module),
    Library(ir::Library),
}

pub struct BuildOutput {
    pub artifact: BuildArtifact,
    pub warnings: Vec<Box<dyn DiagnosticOutput>>,
}

pub fn build(input: BuildInput) -> Result<BuildOutput, BuildError> {
    let mut output_warnings = Vec::new();

    let mut auto_ref_units = Vec::new();

    // map of canonical paths by unit names. each unit must have exactly one source path, to ensure
    // we don't try to load two units with the same name at different locations
    let mut unit_paths: HashMap<IdentPath, PathBuf> = HashMap::new();

    // auto-add system units if we're going beyond parsing
    let will_typecheck = input.output_stage >= BuildStage::Typecheck;

    if will_typecheck {
        auto_ref_units.push(IdentPath::new(builtin_ident(SYSTEM_UNIT_NAME), []));
    }

    let mut sources = SourceCollection::new(&input.search_dirs, input.compile_opts.verbose)?;

    // add builtin units
    for auto_ref_unit in auto_ref_units.into_iter() {
        let unit_filename =
            PathBuf::from(auto_ref_unit.to_string()).with_extension(SRC_FILE_DEFAULT_EXT);

        let unit_path = sources.add(&unit_filename, None)?.canonicalize().map_err(|err| {
            BuildError::ReadSourceFileFailed {
                msg: err.to_string(),
                path: unit_filename,
            }
        })?;

        unit_paths.insert(auto_ref_unit, unit_path);
    }

    // add extra referenced units
    for unit_arg in input.units.iter() {
        let unit_filename = PathBuf::from(unit_arg.clone());
        sources.add(&unit_filename, None)?;
    }

    if input.compile_opts.verbose {
        println!("Unit source directories:");
        for path in sources.source_dirs() {
            println!("\t{}", path.display());
        }
    }

    // if we just want preprocessor output, no unit refs need to be looked up, just process and
    // print the units provided on the cli
    if input.output_stage == BuildStage::Preprocess {
        let mut pp_units = Vec::new();
        while let Some(source_path) = sources.next() {
            let pp_unit = preprocess(&source_path, input.compile_opts.clone())?;
            pp_units.push(pp_unit);
        }
        return Ok(BuildOutput {
            artifact: BuildArtifact::PreprocessedText(pp_units),
            warnings: output_warnings,
        });
    }

    // files parsed in the order specified, either by the project unit or on the command line
    let mut parsed_files: LinkedHashMap<PathBuf, ast::Unit> = LinkedHashMap::new();

    // not actually used for sorting, just an easy way to detect circular deps
    let mut dep_sort = TopologicalSort::<IdentPath>::new();

    let mut main_ident = None;

    loop {
        let unit_filename = match sources.next() {
            None => break,
            Some(f) => f,
        };

        let canon_filename =
            unit_filename.canonicalize().map_err(|e| BuildError::ReadSourceFileFailed {
                msg: e.to_string(),
                path: unit_filename.clone(),
            })?;

        let unit = match parsed_files.get(&canon_filename) {
            Some(unit) => {
                return Err(BuildError::DuplicateUnit {
                    unit_ident: unit.ident.clone(),
                    new_path: unit_filename.clone(),
                    existing_path: unit_filename.clone(),
                });
            },

            None => {
                if input.compile_opts.verbose {
                    eprintln!("parsing unit @ `{}`", unit_filename.display());
                }

                let pp_unit = preprocess(&canon_filename, input.compile_opts.clone())?;

                for warning in &pp_unit.warnings {
                    output_warnings.push(Box::new(warning.clone()));
                }

                let tokens = tokenize(pp_unit)?;
                let parsed_unit = parse(canon_filename.clone(), tokens)?;

                add_unit_path(&parsed_unit.ident, &unit_filename, &mut unit_paths)?;

                dep_sort.insert(parsed_unit.ident.clone());

                parsed_files.insert(canon_filename.clone(), parsed_unit);
                &parsed_files[&canon_filename]
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
                    unit_path: canon_filename,
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
            if !unit_paths.contains_key(&used_unit.ident) {
                if is_main_unit {
                    if input.compile_opts.verbose {
                        println!("unit {} used from {}", used_unit, unit_ident);
                    }

                    let used_unit_path = match used_unit.path {
                        Some(path) => {
                            let filename = PathBuf::from(path);
                            sources.add_used_unit_in_file(
                                &canon_filename,
                                &used_unit.ident,
                                &filename,
                            )?
                        },

                        None => sources.add_used_unit(&canon_filename, &used_unit.ident)?,
                    };

                    add_unit_path(&used_unit.ident, &used_unit_path, &mut unit_paths)?;
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
        path: &PathBuf,
        unit_paths: &mut HashMap<IdentPath, PathBuf>,
    ) -> Result<(), BuildError> {
        let canon_path = path.canonicalize().map_err(|err| BuildError::ReadSourceFileFailed {
            msg: err.to_string(),
            path: path.clone(),
        })?;

        match unit_paths.entry(unit_ident.clone()) {
            Entry::Occupied(occupied_ident_filename) => {
                // the same unit can't be loaded from two separate filenames. it might
                // already be in the filename map because we insert the builtin units ahead
                // of time
                if *occupied_ident_filename.get() != canon_path {
                    Err(BuildError::DuplicateUnit {
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
            },
        }
    }

    let mut compile_units: Vec<_> = parsed_files.into_iter().map(|(_path, unit)| unit).collect();

    if input.output_stage == BuildStage::Parse {
        return Ok(BuildOutput {
            artifact: BuildArtifact::ParsedUnit(compile_units),
            warnings: output_warnings,
        });
    }

    if input.compile_opts.verbose {
        println!("Compilation units:");
        for unit in &compile_units {
            println!("\t{}", unit.ident);
        }
    }

    // reverse the compilation order for typechecking, modules should be processed after all their
    // dependencies
    compile_units.reverse();
    let typed_module = typecheck(&compile_units, input.compile_opts.verbose)?;

    if input.output_stage == BuildStage::Typecheck {
        return Ok(BuildOutput {
            artifact: BuildArtifact::TypedModule(typed_module),
            warnings: output_warnings,
        });
    }

    let library = codegen_ir(&typed_module, input.codegen_opts);

    Ok(BuildOutput { 
        artifact: BuildArtifact::Library(library), 
        warnings: output_warnings 
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
