use crate::compile_error::RunError;
use terapascal_ir as ir;
use crate::Args;

fn dotnet_build(lib: &ir::Library, args: &Args, out_path: &Path) -> Result<(), RunError> {
    backend_cil::build_assembly(lib, out_path, args.verbose)?;
    Ok(())
}