use crate::error::BuildError;
use std::collections::LinkedList;
use std::env;
use std::path::Path;
use std::path::PathBuf;
use terapascal_common::build_log::BuildLog;
use terapascal_common::span::*;
use terapascal_common::SRC_FILE_DEFAULT_EXT;
use terapascal_frontend::ast::IdentPath;

fn find_in_path(filename: &PathBuf, dir: &Path) -> Option<PathBuf> {
    let file_path = dir.join(filename);

    if file_path.exists() {
        file_path.canonicalize().ok()
    } else {
        None
    }
}

fn find_in_paths(filename: &PathBuf, search_paths: &[PathBuf]) -> Option<PathBuf> {
    for search_path in search_paths.iter() {
        if search_path.exists() && search_path.is_dir() {
            if let Some(result_path) = find_in_path(filename, search_path) {
                return Some(result_path);
            }
        }
    }

    None
}

pub struct SourceCollection {
    verbose: bool,

    source_dirs: Vec<PathBuf>,

    // ordered list of sources
    source_list: LinkedList<PathBuf>,
}

impl SourceCollection {
    pub fn new<'a>(search_dirs: &[PathBuf], verbose: bool) -> Result<Self, BuildError> {
        let source_dirs = search_dirs
            .iter()
            .filter(|dir| dir.exists())
            .cloned()
            .chain({
                let mut unit_dirs = Vec::with_capacity(4);
                if let Ok(units_var) = env::var("TERAPASCAL_UNITS") {
                    unit_dirs.extend(units_var.split(";").map(PathBuf::from));
                }

                if let Ok(cwd) = env::current_dir() {
                    unit_dirs.push(cwd);
                }

                unit_dirs
                    .into_iter()
                    .filter(|dir| dir.exists())
            })
            .collect();

        let sources = Self {
            verbose,

            source_dirs,
            source_list: LinkedList::new(),
        };

        Ok(sources)
    }

    pub fn source_dirs(&self) -> &[PathBuf] {
        &self.source_dirs
    }
    
    pub fn find_unit(&self, unit_filename: &PathBuf) -> Option<PathBuf> {
        find_in_paths(unit_filename, &self.source_dirs)
    }

    pub fn add(&mut self, unit_filename: &PathBuf, span: Option<Span>, log: &mut BuildLog) -> Result<PathBuf, BuildError> {
        match find_in_paths(unit_filename, &self.source_dirs) {
            Some(path) => {
                if !self.source_list.contains(&path) {
                    if self.verbose {
                        log.trace(format!("added source path {}", path.display()));
                    }

                    self.source_list.push_back(path.clone());
                }

                Ok(path)
            }

            None => {
                Err(BuildError::FileNotFound(unit_filename.clone(), span))
            },
        }
    }

    pub fn add_used_unit(&mut self,
        base_unit_path: &PathBuf,
        used_unit: &IdentPath,
        log: &mut BuildLog
    ) -> Result<PathBuf, BuildError> {
        let unit_filename = PathBuf::from(used_unit.to_string() + "." + SRC_FILE_DEFAULT_EXT);

        self.add_used_unit_in_file(base_unit_path, used_unit, &unit_filename, log)
    }

    pub fn add_used_unit_in_file(
        &mut self,
        unit_dir: &PathBuf,
        used_unit: &IdentPath,
        filename: &PathBuf,
        log: &mut BuildLog,
    ) -> Result<PathBuf, BuildError> {
        if let Some(unit_dir) = unit_dir.parent() {
            if let Some(used_path) = find_in_path(filename, unit_dir) {
                if self.verbose {
                    log.trace(format!("added source path {} for unit {}", used_path.display(), used_unit));
                }

                self.source_list.push_back(used_path.clone());
                return Ok(used_path);
            }
        }

        self.add(filename, Some(used_unit.path_span()), log)
    }

    pub fn next(&mut self) -> Option<PathBuf> {
        self.source_list.pop_front()
    }
}
