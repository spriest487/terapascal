pub mod error;
pub mod sources;

use crate::error::BuildError;
use crate::error::BuildResult;
use crate::sources::SourceCollection;
use linked_hash_map::LinkedHashMap;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::{env, io};
use terapascal_backend_c::ir;
use terapascal_common::build_log::BuildLog;
use terapascal_common::fs::Filesystem;
use terapascal_common::span::Span;
use terapascal_common::version::Version;
use terapascal_common::CompileOpts;
use terapascal_common::TracedError;
use terapascal_common::IR_LIB_EXT;
use terapascal_common::LIB_DIR_VAR;
use terapascal_frontend::ast;
use terapascal_frontend::ast::package::PackageUnit;
use terapascal_frontend::ast::IdentPath;
use terapascal_frontend::ast::MainUnitKind;
use terapascal_frontend::ast::UnitKind;
use terapascal_frontend::ast::UseDeclItem;
use terapascal_frontend::codegen::CodegenOpts;
use terapascal_frontend::codegen_ir;
use terapascal_frontend::digest::digest;
use terapascal_frontend::digest::DigestOutput;
use terapascal_frontend::parse;
use terapascal_frontend::parse::ParseError;
use terapascal_frontend::parse::Parser;
use terapascal_frontend::pp::PreprocessedUnit;
use terapascal_frontend::tokenize;
use terapascal_frontend::typ;
use terapascal_frontend::typ::Context;
use terapascal_frontend::typecheck;
use terapascal_frontend::TokenStream;
use topological_sort::TopologicalSort;
use terapascal_frontend::codegen::library_builder::LibraryRef;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Hash)]
pub enum BuildStage {
    Preprocess,
    Parse,
    Typecheck,
    Codegen,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BuildInput {
    pub source_path: PathBuf,

    pub search_dirs: Vec<PathBuf>,

    pub package_names: Vec<String>,

    pub project_name: Option<String>,
    pub project_version: Option<Version>,

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
    pub project_name: String,
    pub project_version: Version,
    pub units: LinkedHashMap<PathBuf, ast::Unit>,
}

struct ProjectLoader<'a, Fs: Filesystem> {
    main_unit_name: Option<IdentPath>,

    input: &'a BuildInput,
    log: &'a mut BuildLog,

    // namespaces imported from external libraries, which we won't search for as local unit files
    // when we encounter them as in `using` statements
    imported_namespaces: HashSet<IdentPath>,

    sources: SourceCollection<'a, Fs>,

    // files parsed in the order specified, either by the project unit or on the command line
    parsed_units: LinkedHashMap<PathBuf, ast::Unit>,

    // map of canonical paths by unit names. each unit must have exactly one source path, to ensure
    // we don't try to load two units with the same name at different locations
    used_unit_paths: HashMap<IdentPath, PathBuf>,

    dep_sort: TopologicalSort<IdentPath>,
}

impl<'a, Fs: Filesystem> ProjectLoader<'a, Fs> {
    fn new(
        fs: &'a Fs,
        input: &'a BuildInput,
        imported_namespaces: impl IntoIterator<Item=IdentPath>,
        log: &'a mut BuildLog,
    ) -> BuildResult<Self> {
        let mut sources = SourceCollection::new(fs, &input.search_dirs, input.compile_opts.verbose)?;
        sources.add(&input.source_path, None, log)?;

        let imported_namespaces: HashSet<_> = imported_namespaces.into_iter().collect();

        if input.compile_opts.verbose {
            log.trace("Imported namespaces:");
            for ns in &imported_namespaces {
                log.trace(format!("\t{}", ns));
            }

            log.trace("Unit source directories:");
            for path in sources.source_dirs() {
                log.trace(format!("\t{}", path.display()));
            }
        }

        Ok(Self {
            main_unit_name: None,

            input,
            log,

            sources,

            imported_namespaces,

            parsed_units: LinkedHashMap::new(),

            used_unit_paths: HashMap::new(),
            dep_sort: TopologicalSort::new(),
        })
    }

    fn process_used_units(
        &mut self,
        unit_filename: &PathBuf,
        unit_ident: &IdentPath,
        main_unit_kind: Option<MainUnitKind>,
        uses_units: impl IntoIterator<Item=UseDeclItem>,
    ) -> BuildResult<()> {
        let is_package_unit = matches!(main_unit_kind, Some(MainUnitKind::Package));

        for used_unit in uses_units {
            // if this unit matches a namespace in a loaded library, the unit will be loaded
            // from that library and we shouldn't try to load it from disk
            if self.imported_namespaces.contains(&used_unit.ident) {
                continue;
            }

            // only project units can load other units. if this isn't the main project unit,
            // the unit we're trying to load must already have been declared in the project unit
            // or a package it references
            if !self.used_unit_paths.contains_key(&used_unit.ident) {
                if main_unit_kind.is_none() {
                    return Err(BuildError::UnitNotLoaded {
                        unit_name: used_unit.ident.clone(),
                    });
                }

                if self.input.compile_opts.verbose {
                    if is_package_unit {
                        self.log.trace(format!("package {} contains unit {}", unit_filename.display(), used_unit));
                    } else {
                        self.log.trace(format!("unit {} uses unit {}", unit_filename.display(), used_unit));
                    }
                }

                let used_unit_path = match used_unit.path {
                    Some(path) => {
                        let filename = PathBuf::from(path);
                        self.sources.add_used_unit_in_file(
                            &unit_filename,
                            &used_unit.ident,
                            &filename,
                            self.log,
                        )?
                    },

                    None => {
                        self.sources.add_used_unit(&unit_filename, &used_unit.ident, self.log)?
                    },
                };

                self.add_unit_path(&used_unit.ident, &used_unit_path)?;
            }

            let is_package_unit = matches!(main_unit_kind, Some(MainUnitKind::Package));
            if !is_package_unit {
                self.add_unit_dep(&unit_ident, &used_unit.ident)?;
            }
        }

        Ok(())
    }

    fn add_unit_dep(
        &mut self,
        unit_ident: &IdentPath,
        used_unit: &IdentPath,
    ) -> BuildResult<()> {
        if self.input.compile_opts.verbose {
            self.log.trace(format!("Unit dependency: {unit_ident} -> {used_unit}"));
        }

        self.dep_sort.add_dependency(used_unit.clone(), unit_ident.clone());

        // check for cycles
        if self.dep_sort.peek().is_none() {
            return Err(BuildError::CircularDependency {
                unit_ident: unit_ident.clone(),
                used_unit: used_unit.clone(),
                span: used_unit.path_span(),
            });
        }

        Ok(())
    }

    fn add_unit_path(&mut self,
        unit_ident: &IdentPath,
        unit_path: &PathBuf,
    ) -> Result<(), BuildError> {
        match self.used_unit_paths.entry(unit_ident.clone()) {
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
                if self.input.compile_opts.verbose {
                    self.log.trace(format!("{}: adding filename {}", vacant_ident_filename.key(), unit_path.display()))
                }

                vacant_ident_filename.insert(unit_path.clone());

                Ok(())
            },
        }
    }

    fn try_set_main_unit(&mut self,
        unit_path: &PathBuf,
        unit_ident: &IdentPath,
        kind: MainUnitKind,
    ) -> BuildResult<()> {
        if let Some(prev_ident) = &self.main_unit_name {
            return Err(BuildError::UnexpectedMainUnit {
                existing_ident: Some(prev_ident.clone()),
                unit_path: unit_path.clone(),
                unit_kind: kind,
            });
        }

        self.main_unit_name = Some(unit_ident.clone());
        Ok(())
    }

    fn version(&self) -> Version {
        self.input.project_version.unwrap_or_else(|| Version::new(0, 1, 0))
    }

    fn name(&mut self) -> BuildResult<String> {
        match self.input.project_name.clone() {
            None => {
                self.project_name_from_main_unit()
            }

            Some(name) if name.is_empty() || name.chars().all(char::is_whitespace) => {
                self.project_name_from_main_unit()
            }

            Some(name) => {
                Ok(name.trim().to_string())
            },
        }
    }

    fn project_name_from_main_unit(&mut self) -> BuildResult<String> {
        if let Some(name) = &self.main_unit_name {
            return Ok(name.to_string());
        }

        let unit_filename = self.input.source_path.file_stem().ok_or_else(|| {
            BuildError::ReadSourceFileFailed {
                path: self.input.source_path.clone(),
                msg: "unable to determine project name from unit path".to_string(),
            }
        })?;

        let name_from_filename = unit_filename.to_string_lossy().to_string();
        if self.input.compile_opts.verbose {
            self.log.trace(format!("no main unit found, inferring project name from filename: {}", name_from_filename));
        }
        Ok(name_from_filename)
    }
}

pub fn preprocess_project(fs: &impl Filesystem, input: &BuildInput, log: &mut BuildLog) -> BuildResult<Vec<PreprocessedUnit>> {
    let mut loader = ProjectLoader::new(fs, input, [], log)?;

    let mut pp_units = Vec::new();
    while let Some(source_path) = loader.sources.next() {
        let pp_unit = preprocess(fs, &source_path, input.compile_opts.clone())?;
        pp_units.push(pp_unit);
    }

    Ok(pp_units)
}

pub fn parse_sources(
    fs: &impl Filesystem,
    input: &BuildInput,
    imported_namespaces: impl IntoIterator<Item=IdentPath>,
    log: &mut BuildLog,
) -> BuildResult<ParseOutput> {
    let verbose = input.compile_opts.verbose;

    let mut project = ProjectLoader::new(fs, input, imported_namespaces, log)?;

    // auto-add system units if we're going beyond parsing
    // let include_system = input.output_stage >= BuildStage::Typecheck;

    // let system_units = [
    //     IdentPath::from_parts([builtin_ident(SYSTEM_UNIT_NAME)])
    // ];

    // let mut system_paths = Vec::with_capacity(system_units.len());

    // if include_system {
    //     for stdlib_unit in &system_units {
    //         let filename = PathBuf::from(stdlib_unit.to_string())
    //             .with_extension(SRC_FILE_DEFAULT_EXT);
    //
    //         let unit_path = project.sources.add(&filename, None, &mut project.log)?;
    //
    //         system_paths.push(unit_path);
    //     }
    // }

    // for (unit_name, path) in system_units.iter().zip(system_paths.into_iter()) {
    //     project.used_unit_paths.insert(unit_name.clone(), path);
    // }

    loop {
        let unit_filename = match project.sources.next() {
            None => break,
            Some(f) => f,
        };

        if !project.parsed_units.contains_key(&unit_filename) {
            if verbose {
                project.log.trace(format!("parsing unit @ `{}`", unit_filename.display()));
            }

            let pp_unit = preprocess(fs, &unit_filename, input.compile_opts.clone())?;

            for warning in pp_unit.warnings.clone() {
                project.log.diagnostic(warning);
            }

            let tokens = tokenize(pp_unit)?;

            if PackageUnit::is_package(&tokens) {
                let file_span = Span::zero(unit_filename.clone());
                let parser = Parser::new(TokenStream::new(tokens, file_span));
                let package_unit = PackageUnit::parse(parser)?;

                project.try_set_main_unit(&unit_filename, &package_unit.name, MainUnitKind::Package)?;

                if let Some(package_contains) = package_unit.contains {
                    project.process_used_units(
                        &unit_filename,
                        &package_unit.name,
                        Some(MainUnitKind::Package),
                        package_contains.units,
                    )?;
                }
                continue;
            }

            // if the unit is parsed to completion with errors, add the errors to the log
            // and continue with the partially parsed unit
            let parsed_unit = match parse(unit_filename.clone(), tokens) {
                Ok(unit) => unit,
                Err(err) => match err.err {
                    ParseError::UnitWithErrors(err) => {
                        let (unit, errors) = err.unwrap();
                        for error in errors {
                            project.log.diagnostic(error);
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

            project.add_unit_path(&parsed_unit.ident, &unit_filename)?;
            project.dep_sort.insert(parsed_unit.ident.clone());

            project.parsed_units.insert(unit_filename.clone(), parsed_unit);

            let unit = &project.parsed_units[&unit_filename];
            if verbose {
                project.log.trace(format!("{}: parsed unit @ {}", unit.ident, unit_filename.display()));
            }

            let is_main_unit = MainUnitKind::try_from(unit.kind).is_ok();
            let unit_ident = unit.ident.clone();
            let unit_kind = unit.kind;

            let uses_units: Vec<_> = unit
                .all_decls()
                .filter_map(|(_vis, decl)| match decl {
                    ast::UnitDecl::Uses { decl } => Some(decl.clone()),
                    _ => None,
                })
                .flat_map(|uses| uses.units)
                .collect();

            project.process_used_units(
                &unit_filename,
                &unit_ident,
                MainUnitKind::try_from(unit_kind).ok(),
                uses_units,
            )?;

            if is_main_unit {
                project.try_set_main_unit(&unit_filename, &unit_ident, match unit_kind {
                    UnitKind::Library => MainUnitKind::Library,
                    _ => MainUnitKind::Program,
                })?;
            }
        }
    }

    let project_name = project.name()?;
    let project_version = project.version();
    
    let mut sorted_unit_names: Vec<_> = project.dep_sort.collect();
    sorted_unit_names.reverse();

    let mut sorted_units = LinkedHashMap::new();    
    for unit_name in sorted_unit_names {
        let unit_path = &project.used_unit_paths[&unit_name];
        let unit = project.parsed_units.remove(unit_path).ok_or_else(|| {
            let err = format!("unit {} was found in the dependency map but no parsed unit was present in the output", unit_path.display());
            BuildError::InternalError(err)
        })?;
        sorted_units.insert(unit_path.clone(), unit);
    }

    if verbose {
        project.log.trace("Compilation units:");
        for (unit_path, unit) in &sorted_units {
            project.log.trace(format!("\t{} in '{}'", unit.ident, unit_path.display()));
        }
    }

    Ok(ParseOutput {
        project_name,
        project_version,
        units: sorted_units,
    })
}

pub fn build(fs: &impl Filesystem, input: BuildInput) -> BuildOutput {
    let mut log = BuildLog::new();

    let artifact = build_with_log(fs, input, &mut log);

    BuildOutput {
        artifact,
        log,
    }
}

fn load_package(
    name: &str,
    input: &BuildInput,
    type_ctx: Option<&mut Context>,
) -> BuildResult<DigestOutput> {
    let mut search_dirs = Vec::new();

    if let Some(current_dir) = input.source_path.parent() {
        search_dirs.push(current_dir.to_path_buf());
    }

    if let Ok(lib_path) = env::var(LIB_DIR_VAR) {
        search_dirs.push(PathBuf::from(lib_path));
    }

    let path = search_dirs
        .iter()
        .find_map(|dir| {
            let full_path = dir.join(name).with_added_extension(IR_LIB_EXT);
            full_path.exists().then_some(full_path)
        })
        .ok_or_else(|| {
            let msg = format!("failed to locate library {name}.{IR_LIB_EXT}");
            io::Error::new(io::ErrorKind::NotFound, msg)
        })?;

    let digest = digest(path.as_path(), type_ctx)?;

    Ok(digest)
}

fn build_with_log(
    fs: &impl Filesystem,
    input: BuildInput,
    log: &mut BuildLog,
) -> BuildResult<BuildArtifact> {
    // if we just want preprocessor output, no unit refs need to be looked up, just process and
    // print the units provided on the cli in that order
    if input.output_stage == BuildStage::Preprocess {
        let units = preprocess_project(fs, &input, log)?;
        return Ok(BuildArtifact::PreprocessedText(units));
    }

    let mut root_ctx = (input.output_stage >= BuildStage::Typecheck)
        .then(|| Context::root(input.compile_opts.clone()));

    let mut package_namespaces = HashSet::new();
    let mut package_libs = Vec::with_capacity(input.package_names.len());

    for package_name in &input.package_names {
        let package_digest = load_package(&package_name, &input, root_ctx.as_mut())?;
        for warning in package_digest.warnings {
            log.diagnostic(warning);
        }

        package_namespaces.extend(package_digest.namespaces.clone());

        package_libs.push(LibraryRef {
            lib: package_digest.library,
            imported_funcs: package_digest.imported_funcs,
        });
    }

    let parse_output = parse_sources(fs, &input, package_namespaces, log)?;

    if input.output_stage == BuildStage::Parse {
        return Ok(BuildArtifact::ParsedUnits(parse_output));
    }

    let mut root_ctx = root_ctx.unwrap();

    // reverse the compilation order for typechecking, modules should be processed after all their
    // dependencies
    let typed_module = typecheck(
        parse_output.project_name,
        parse_output.project_version,
        parse_output.units.iter(),
        &mut root_ctx,
        log,
    );

    if input.output_stage == BuildStage::Typecheck {
        return Ok(BuildArtifact::TypedModule(typed_module));
    }

    if log.has_errors() {
        return Err(BuildError::CompletedWithErrors);
    }

    let library = codegen_ir(&typed_module, &root_ctx, package_libs, input.codegen_opts);

    Ok(BuildArtifact::Library(library))
}

fn preprocess(fs: &impl Filesystem, filename: &PathBuf, opts: CompileOpts) -> Result<PreprocessedUnit, BuildError> {
    let src = fs.read_source(filename).map_err(|err| BuildError::ReadSourceFileFailed {
        path: filename.to_path_buf(),
        msg: err.to_string(),
    })?;

    let preprocessed = terapascal_frontend::preprocess(fs, filename, &src, opts)?;
    Ok(preprocessed)
}