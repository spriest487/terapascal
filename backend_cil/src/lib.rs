use std::env;
use std::io;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::ExitStatus;
use std::process::Stdio;
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
    
    #[error("build failed with status {0}")]
    BuildFailed(ExitStatus),
}

pub fn build_assembly(
    lib: &ir::Library,
    out_path: &Path,
    verbose: bool,
) -> Result<(), BuildError> {
    let encoded_data = ir::encode_lib(lib)
        .map_err(BuildError::EncodingFailed)?;

    let tool_dir = env::current_exe()
        .and_then(|exe_path| {
            let parent = exe_path.parent().ok_or_else(|| {
                let msg = format!("unable to get directory from executable path {}", exe_path.display());
                io::Error::new(io::ErrorKind::InvalidFilename, msg)
            })?;
            Ok(parent.join("cil"))
        })
        .map_err(BuildError::FindDirFailed)?;

    let mut out_path = out_path.to_path_buf();
    if !out_path.is_absolute() {
        let cwd = env::current_dir()?;
        out_path = cwd.join(out_path);
    }
    
    if verbose {
        println!("terapascal2cil tool directory: {}", tool_dir.display());
        println!("terapascal2cil output path: {}", out_path.display());
    }
    
    let mut tool_command = Command::new(tool_dir.join("terapascal2cil"));

    tool_command.current_dir(tool_dir);
    tool_command.arg("--module-kind").arg("Console");
    tool_command.arg("--output-path").arg(out_path);
    
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

    let status = process.wait()?;
    if status.success() {
        Ok(())
    } else {
        Err(BuildError::BuildFailed(status))
    }
}
