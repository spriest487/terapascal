use std::path::Path;
use crate::error::RunError;
use crate::Args;
use terapascal_ir as ir;
use terapascal_backend_net as backend_net;

pub(crate) fn dotnet_build(lib: &ir::Library, args: &Args, out_path: &Path) -> Result<(), RunError> {
    backend_net::build_assembly(lib, out_path, args.verbose)?;
    Ok(())
}