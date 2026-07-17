use std::fs::DirEntry;
use std::io;
use std::path::{Path, PathBuf};
use terapascal_common::SRC_FILE_DEFAULT_EXT;
use crate::test_script::TestScript;

#[derive(Clone)]
pub struct TestCase {
    pub path: PathBuf,
    pub script: TestScript,
}

impl TestCase {
    pub fn working_dir(&self) -> &Path {
        self.path.parent().expect("source file must have a parent directory")
    }

    fn find_from_entry(entry: DirEntry) -> io::Result<Vec<TestCase>> {
        let file_type = entry.file_type()?;

        if file_type.is_dir() {
            return Ok(TestCase::find_at_path(&entry.path()));
        }

        let file_path = entry.path();

        let paths = if let Some(ext) = file_path.extension()
            && ext == SRC_FILE_DEFAULT_EXT
            && let Some(case) = Self::read_file(&file_path)
        {
            vec![case]
        } else {
            Vec::new()
        };

        Ok(paths)
    }

    fn read_file(path: &Path) -> Option<TestCase> {
        let script = TestScript::find_for_path(&path)
            .unwrap_or_else(|err| {
                eprintln!("failed to read test script: {err}");
                TestScript::default()
            });

        if script.ignore {
            None
        } else {
            Some(TestCase { path: path.to_path_buf(), script })
        }
    }

    pub fn find_at_path(root: &Path) -> Vec<TestCase> {
        if root.is_file() {
            return Self::read_file(root).into_iter().collect();
        }

        root.read_dir()
            .ok()
            .map(|read_dir|
                read_dir
                    .filter_map(|read_entry| {
                        read_entry
                            .and_then(TestCase::find_from_entry)
                            .ok()
                    })
                    .flatten()
                    .filter(|case| !case.script.ignore)
                    .collect()
            )
            .unwrap_or_default()
    }
}