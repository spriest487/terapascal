use crate::fs::WorkspaceFilesystem;
use crate::project::BuildDiagnostics;
use crate::project::Project;
use std::collections::HashMap;
use std::path::PathBuf;
use terapascal_build::error::BuildError;
use terapascal_build::error::BuildResult;
use terapascal_common::fs::Filesystem;
use tower_lsp::lsp_types::Url;

pub struct Workspace {
    projects: HashMap<PathBuf, Project>,
    document_projects: HashMap<PathBuf, PathBuf>,

    filesystem: WorkspaceFilesystem,
}

impl Workspace {
    pub fn new() -> Self {
        Workspace {
            filesystem: WorkspaceFilesystem::new(),
            projects: HashMap::new(),
            document_projects: HashMap::new(),
        }
    }

    pub fn get_document_project(&self, document_path: &PathBuf) -> Option<&Project> {
        self.projects.get(self.document_projects.get(document_path)?)
    }

    fn remove_project_units(&mut self, project_path: &PathBuf) {
        for (_doc_path, doc_proj_path) in &self.document_projects {
            if doc_proj_path == project_path {
                self.filesystem.remove(doc_proj_path);
            }
        }
        
        self.document_projects.retain(|_doc_path, doc_proj_path| {
            doc_proj_path != project_path
        });
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

    pub fn update_document(&mut self, doc_path: &PathBuf, source: String, version: i32) -> BuildDiagnostics {
        self.filesystem.add(doc_path.clone(), source, version);
        
        let project_path = self.document_projects.get(doc_path).cloned();

        if let Some(project_path) = project_path {
            let project = self.projects.get_mut(&project_path).unwrap();
            let diagnostics = project.build(&self.filesystem);

            let unit_paths = project.unit_paths();
            self.remove_project_units(&project_path);
            self.add_project_units(&project_path, unit_paths);

            diagnostics
        } else {
            eprintln!("[update_document] creating project {}", doc_path.display());

            // load the current unit as a project
            // todo: should be able to search for/specify the root project file
            let mut project = Project::new(doc_path.clone());
            let diagnostics = project.build(&self.filesystem);

            self.add_project_units(&doc_path, project.unit_paths());
            self.projects.insert(doc_path.clone(), project);

            diagnostics
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
