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

        let paths = if file_type.is_dir() {
            TestCase::find_at_path(&entry.path())
        } else {
            let path = entry.path();

            match path.extension() {
                Some(ext) if ext.eq_ignore_ascii_case(SRC_FILE_DEFAULT_EXT) => {
                    let script = TestScript::find_for_path(&path)
                        .unwrap_or_else(|err| {
                            eprintln!("failed to read test script: {err}");
                            TestScript::default()
                        });

                    if script.ignore {
                        Vec::new()
                    } else {
                        vec![TestCase { path, script }]
                    }
                }

                _ => Vec::new(),
            }
        };

        Ok(paths)
    }

    pub fn find_at_path(root: &Path) -> Vec<TestCase> {
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