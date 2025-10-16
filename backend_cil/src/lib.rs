use std::io;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::process::Stdio;
use terapascal_build::encode_lib;
use terapascal_ir as ir;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum BuildError {
    #[error(transparent)]
    IOError(#[from] io::Error),
}

pub fn build_assembly( name: impl Into<String>, lib: &ir::Library, path: &Path) -> Result<(), BuildError> {
    let encoded_data = encode_lib(lib)?;
    let name = name.into();

    let mut process = Command::new("terapascal2cil")
        .arg("--assembly-name").arg(name)
        .arg("--output-path").arg(path)
        .stdin(Stdio::piped())
        .spawn()?;
    
    let mut input = process.stdin
        .take()
        .expect("piped stdin was specified");

    input.write_all(&encoded_data)?;
    drop(input);
    
    Ok(())
}
