mod definition_map;

use crate::fs::WorkspaceFilesystem;
pub use crate::project::definition_map::DefinitionMap;
pub use crate::project::definition_map::LinksEntry;
use crate::semantic_tokens::SemanticTokenBuilder;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use terapascal_build::error::BuildError;
use terapascal_build::parse_units;
use terapascal_build::BuildInput;
use terapascal_build::BuildStage;
use terapascal_build::ParseOutput;
use terapascal_common::build_log::BuildLog;
use terapascal_common::span::Location;
use terapascal_common::span::Spanned;
use terapascal_common::{CompileOpts, Severity};
use terapascal_common::DiagnosticMessage;
use terapascal_common::DiagnosticOutput;
use terapascal_frontend::codegen::CodegenOpts;
use terapascal_frontend::typ;
use terapascal_frontend::typecheck;
use tower_lsp::lsp_types::SemanticToken;

pub struct FileDiagnostics {
    pub errors: Vec<DiagnosticMessage>,
    pub version: Option<i32>,
}

pub struct BuildDiagnostics {
    pub files: HashMap<PathBuf, FileDiagnostics>,
}

impl BuildDiagnostics {
    pub fn add_err(&mut self, err: impl Into<BuildError>, filesystem: &WorkspaceFilesystem) {
        let err = err.into();
        
        self.add_message(err.main(Severity::Error), filesystem);
        for message in err.see_also() {
            self.add_message(message, filesystem);
        }
    }
    
    fn add_message(&mut self, message: DiagnosticMessage, filesystem: &WorkspaceFilesystem) {
        let Some(path) = message_path(&message) else {
            return;
        };
        
        let version = filesystem.file_version(&path);

        self.file_diagnostics(path, version).errors.push(message);
    }

    pub fn file_diagnostics(
        &mut self,
        path: PathBuf,
        version: Option<i32>,
    ) -> &mut FileDiagnostics {
        self.files.entry(path).or_insert_with(|| FileDiagnostics {
            errors: Vec::new(),
            version,
        })
    }
}

fn message_path(message: &DiagnosticMessage) -> Option<PathBuf> {
    let label = message.label.as_ref()?;
    let path = label.span.file.to_path_buf();

    Some(path)
}

pub struct Project {
    main_file: PathBuf,

    pub semantic_tokens: HashMap<Arc<PathBuf>, Vec<SemanticToken>>,

    parse_output: Option<ParseOutput>,
    module: Option<typ::Module>,

    definition_map: DefinitionMap,
}

impl Project {
    pub fn new(main_file: PathBuf) -> Self {
        Project {
            main_file,

            semantic_tokens: HashMap::new(),

            module: None,
            parse_output: None,

            definition_map: DefinitionMap::empty(),
        }
    }

    pub fn build(&mut self, filesystem: &WorkspaceFilesystem) -> BuildDiagnostics {
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

        let mut diagnostics = BuildDiagnostics {
            files: HashMap::new(),
        };

        match parse_units(filesystem, &input, &mut log) {
            Ok(parsed_output) => {
                match typecheck(
                    parsed_output.units.iter(),
                    input.compile_opts.verbose,
                    &mut log,
                ) {
                    Ok(module) => {
                        for unit in &module.units {
                            let version = filesystem.file_version(unit.path.as_ref());
                            diagnostics.file_diagnostics(unit.path.to_path_buf(), version);

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
                        eprintln!("[build] {} ({})", err.main(Severity::Error), err.span());
                        diagnostics.add_err(err, filesystem);

                        for (unit_path, unit) in &parsed_output.units {
                            let version = filesystem.file_version(unit_path);
                            diagnostics.file_diagnostics(unit_path.to_path_buf(), version);

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
                eprintln!("[build] {} ({})", parse_err.main(Severity::Error), parse_err.span());

                diagnostics.add_err(parse_err, filesystem);
            },

            Err(err) => {
                eprintln!("[build] {}", err.main(Severity::Error));
                diagnostics.add_err(err, filesystem);
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

        diagnostics
    }

    pub fn unit_paths(&self) -> Vec<PathBuf> {
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
