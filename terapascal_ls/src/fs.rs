use std::borrow::Cow;
use std::collections::HashMap;
use std::path::PathBuf;
use terapascal_common::fs::DefaultFilesystem;
use terapascal_common::fs::Filesystem;

pub struct WorkspaceFilesystem {
    files: HashMap<PathBuf, String>,
    default_fs: DefaultFilesystem,
}

impl WorkspaceFilesystem {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            default_fs: DefaultFilesystem,
        }
    }
    
    pub fn add(&mut self, path: PathBuf, src: String) {
        self.files.insert(path, src);
    }
    
    pub fn remove(&mut self, path: &PathBuf) {
        self.files.remove(path);
    }
}

impl Filesystem for WorkspaceFilesystem {
    fn read_source(&self, path: &PathBuf) -> std::io::Result<Cow<str>> {
        if let Some(src) = self.files.get(path) {
            Ok(Cow::Borrowed(src.as_str()))
        } else {
            self.default_fs.read_source(path)
        }
    }

    fn exists(&self, path: &PathBuf) -> bool {
        if self.files.contains_key(path) {
            true
        } else {
            self.default_fs.exists(path)
        }
    }

    fn is_dir(&self, path: &PathBuf) -> bool {
        self.default_fs.is_dir(path)
    }

    fn canonicalize(&self, path: &PathBuf) -> std::io::Result<PathBuf> {
        self.default_fs.canonicalize(path)
    }
}
