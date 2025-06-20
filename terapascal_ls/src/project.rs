mod definition_map;

use crate::fs::WorkspaceFilesystem;
pub use crate::project::definition_map::DefinitionMap;
pub use crate::project::definition_map::LinksEntry;
use crate::semantic_tokens::SemanticTokenBuilder;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_build::error::BuildError;
use terapascal_build::error::BuildResult;
use terapascal_build::parse_units;
use terapascal_build::BuildInput;
use terapascal_build::BuildStage;
use terapascal_build::ParseOutput;
use terapascal_common::build_log::BuildLog;
use terapascal_common::fs::Filesystem;
use terapascal_common::span::Location;
use terapascal_common::span::Spanned;
use terapascal_common::CompileOpts;
use terapascal_common::DiagnosticOutput;
use terapascal_frontend::codegen::CodegenOpts;
use terapascal_frontend::typ;
use terapascal_frontend::typecheck;
use tower_lsp::lsp_types::SemanticToken;
use tower_lsp::lsp_types::Url;

pub struct Workspace {
    projects: HashMap<PathBuf, Project>,
    document_projects: HashMap<PathBuf, PathBuf>,

    pub filesystem: WorkspaceFilesystem,
}

impl Workspace {
    pub fn get_document_project(&self, document_path: &PathBuf) -> Option<&Project> {
        self.projects.get(self.document_projects.get(document_path)?)
    }

    fn remove_project_units(&mut self, project_path: &PathBuf) {
        self.document_projects.retain(|_doc_path, doc_proj_path| doc_proj_path != project_path);
    }

    fn add_project_units(
        &mut self,
        project_path: &PathBuf,
        unit_paths: impl IntoIterator<Item = PathBuf>,
    ) {
        eprintln!(
            "[add_project_units] units in project {}:",
            project_path.display()
        );

        for unit_path in unit_paths.into_iter() {
            eprintln!(" - {}", unit_path.display());
            self.document_projects.insert(unit_path, project_path.clone());
        }
    }

    pub fn remove_project(&mut self, project_path: &PathBuf) {
        if self.projects.remove(project_path).is_some() {
            self.remove_project_units(project_path);

            eprintln!("[remove_project] removed {}", project_path.display());
        }
    }

    pub fn update_document(&mut self, doc_path: &PathBuf) {
        let project_path = self.document_projects.get(doc_path).cloned();

        if let Some(project_path) = project_path {
            let project = self.projects.get_mut(&project_path).unwrap();
            project.build(&self.filesystem);

            let unit_paths = project.unit_paths();
            self.remove_project_units(&project_path);
            self.add_project_units(&project_path, unit_paths);
        } else {
            eprintln!("[update_document] creating project {}", doc_path.display());

            // load the current unit as a project
            // todo: should be able to search for/specify the root project file
            let mut project = Project::new(doc_path.clone());
            project.build(&self.filesystem);

            self.add_project_units(&doc_path, project.unit_paths());
            self.projects.insert(doc_path.clone(), project);
        }
    }

    pub fn url_to_path(&self, url: &Url) -> BuildResult<PathBuf> {
        let Ok(file_path) = url.to_file_path() else {
            return Err(BuildError::ReadSourceFileFailed {
                path: PathBuf::from(url.to_string()),
                msg: "invalid file path".to_string(),
            });
        };

        match self.filesystem.canonicalize(&file_path) {
            Ok(path) => Ok(path),

            Err(err) => Err(BuildError::ReadSourceFileFailed {
                path: file_path,
                msg: err.to_string(),
            }),
        }
    }
}

pub struct Project {
    main_file: PathBuf,

    pub semantic_tokens: HashMap<Arc<PathBuf>, Vec<SemanticToken>>,

    parse_output: Option<ParseOutput>,
    module: Option<typ::Module>,

    definition_map: DefinitionMap,
}

impl Project {
    fn new(main_file: PathBuf) -> Self {
        Project {
            main_file,

            semantic_tokens: HashMap::new(),

            module: None,
            parse_output: None,

            definition_map: DefinitionMap::empty(),
        }
    }

    fn build(&mut self, filesystem: &impl Filesystem) {
        self.parse_output = None;
        self.module = None;

        self.semantic_tokens.clear();

        let mut opts = CompileOpts::default();
        opts.verbose = true;

        // todo: make search dirs and compiler options configurable
        let input = BuildInput {
            units: vec![self.main_file.clone()],
            compile_opts: opts,
            codegen_opts: CodegenOpts::default(),
            output_stage: BuildStage::Codegen,
            search_dirs: Vec::new(),
        };

        let mut log = BuildLog::new();

        let result = match parse_units(filesystem, &input, &mut log) {
            Ok(parsed_output) => {
                match typecheck(
                    parsed_output.units.iter(),
                    input.compile_opts.verbose,
                    &mut log,
                ) {
                    Ok(module) => {
                        for unit in &module.units {
                            eprintln!(
                                "[build] processing unit: {} ({})",
                                unit.unit.ident,
                                unit.path.display()
                            );

                            let unit_path = unit.path.clone();

                            let mut token_builder =
                                SemanticTokenBuilder::new(unit_path.clone(), 0, 0);
                            token_builder.add_unit(&unit.unit);

                            let tokens = token_builder.finish();
                            self.semantic_tokens.insert(unit_path, tokens);
                        }

                        self.module = Some(module);
                    },

                    Err(err) => {
                        eprintln!("[build] {} ({})", err.main(), err.span());

                        for (unit_path, unit) in &parsed_output.units {
                            eprintln!(
                                "[build] processing unit: {} ({})",
                                unit.ident,
                                unit_path.display()
                            );

                            let unit_path = Arc::new(unit_path.clone());

                            let mut token_builder =
                                SemanticTokenBuilder::new(unit_path.clone(), 0, 0);
                            token_builder.add_unit(unit);

                            let tokens = token_builder.finish();
                            self.semantic_tokens.insert(unit_path, tokens);
                        }
                    },
                }

                self.parse_output = Some(parsed_output);
            },

            Err(BuildError::ParseError(parse_err)) => {
                eprintln!("[build] {} ({})", parse_err.err.main(), parse_err.span());
            },

            Err(err) => {
                eprintln!("[build] {}", err.main());
            },
        };

        for log_entry in log.entries {
            eprintln!("[build] {}", log_entry);
        }

        self.definition_map = DefinitionMap::from_project(self);
        eprintln!(
            "[build] updated definition map ({} entries)",
            self.definition_map.count_entries()
        );

        result
    }

    fn unit_paths(&self) -> Vec<PathBuf> {
        let mut paths = Vec::new();

        if let Some(parse_output) = &self.parse_output {
            for (unit_path, _) in &parse_output.units {
                paths.push(unit_path.clone());
            }
        } else {
            paths.push(self.main_file.clone());
        }

        paths
    }

    pub fn find_definition(&self, file: &PathBuf, at: Location) -> Option<&LinksEntry> {
        self.definition_map.find_definitions(file, at)
    }

    pub fn find_usages(&self, file: &PathBuf, at: Location) -> Option<&LinksEntry> {
        self.definition_map.find_usages(file, at)
    }
}

impl Workspace {
    pub fn new() -> Self {
        Workspace {
            filesystem: WorkspaceFilesystem::new(),
            projects: HashMap::new(),
            document_projects: HashMap::new(),
        }
    }
}
