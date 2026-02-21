use std::path::Path;
use crate::error::RunError;
use crate::Args;
use terapascal_ir as ir;
use terapascal_backend_cil as backend_cil;

pub(crate) fn dotnet_build(lib: &ir::Library, args: &Args, out_path: &Path) -> Result<(), RunError> {
    backend_cil::build_assembly(lib, out_path, args.verbose)?;
    Ok(())
}