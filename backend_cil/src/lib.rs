use std::env;
use std::io;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::Stdio;
use terapascal_build::encode_lib;
use terapascal_ir as ir;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum BuildError {
    #[error("encoding library failed: {0}")]
    EncodingFailed(io::Error),
    
    #[error("unable to terapascal2cil directory: {0}")]
    FindDirFailed(io::Error),

    #[error("running terapascal2cil failed: {0}")]
    DotnetRunFailed(io::Error),
    
    #[error("unable to determine assembly name from path {}", .0.display())]
    BadAssemblyPath(PathBuf),

    #[error(transparent)]
    IOError(#[from] io::Error),
}

pub fn build_assembly(
    lib: &ir::Library,
    out_path: &Path,
    verbose: bool,
) -> Result<(), BuildError> {
    let encoded_data = encode_lib(lib)
        .map_err(BuildError::EncodingFailed)?;

    let lib_name = out_path
        .file_stem()
        .ok_or_else(|| {
            BuildError::BadAssemblyPath(out_path.to_path_buf())
        })?
        .to_string_lossy()
        .to_string();

    let tool_dir = env::current_exe()
        .and_then(|exe_path| {
            let parent = exe_path.parent().ok_or_else(|| {
                let msg = format!("unable to get directory from executable path {}", exe_path.display());
                io::Error::new(io::ErrorKind::InvalidFilename, msg)
            })?;
            Ok(parent.join("cil"))
        })
        .map_err(BuildError::FindDirFailed)?;
    
    let output_path = out_path.canonicalize()?;
    
    if verbose {
        println!("terapascal2cil tool directory: {}", tool_dir.display());
        println!("terapascal2cil output path: {}", output_path.display());
    }
    
    let mut tool_command = Command::new(tool_dir.join("terapascal2cil"));

    tool_command.current_dir(tool_dir);
    tool_command.arg("--assembly-name").arg(lib_name);
    tool_command.arg("--module-kind").arg("Console");
    tool_command.arg("--output-path").arg(output_path);
    
    if verbose {
        tool_command.arg("--verbose");
    }
    
    tool_command.stdin(Stdio::piped());
    tool_command.stdout(Stdio::inherit());
    tool_command.stderr(Stdio::inherit());

    let mut process = tool_command
        .spawn()
        .map_err(BuildError::DotnetRunFailed)?;
    
    let mut input = process.stdin
        .take()
        .expect("piped stdin was specified");

    input.write_all(&encoded_data)?;
    drop(input);

    process.wait()?;

    Ok(())
}
