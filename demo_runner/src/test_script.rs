use std::fs::File;
use std::io;
use std::path::Path;
use serde::Deserialize;

#[derive(Deserialize, Default, Clone)]
pub struct TestScriptStep {
    #[serde(default)]
    pub input: Option<String>,

    #[serde(default)]
    pub output: Option<String>,

    #[serde(default)]
    pub error_regex: Option<String>,
}

#[derive(Deserialize, Default, Clone)]
pub struct TestScript {
    #[serde(default)]
    pub ignore: bool,

    #[serde(default)]
    pub steps: Vec<TestScriptStep>,
}

impl TestScript {
    pub fn find_for_path(test_path: &Path) -> io::Result<TestScript> {
        let mut script_path = test_path.to_path_buf();
        script_path.set_extension("test.json");

        let script = if script_path.exists() {
            let file = File::open(&script_path)?;
            serde_json::from_reader(file)?
        } else {
            TestScript::default()
        };

        Ok(script)
    }
}
