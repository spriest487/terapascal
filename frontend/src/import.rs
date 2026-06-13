mod read_function;
mod read_type;
mod error;
mod builder;

pub use self::error::*;
use crate::ast::IdentPath;
use crate::codegen::library_builder::FunctionDeclKey;
use crate::codegen::FunctionInstance;
use crate::import::builder::ImportBuilder;
use crate::ir;
use crate::typ::Context;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::io::Read;
use std::path::Path;
use terapascal_ir::decode_lib;

#[derive(Debug)]
pub struct ImportedLibrary {
    pub library: ir::Library,
    pub imported_funcs: HashMap<FunctionDeclKey, FunctionInstance>,

    pub warnings: Vec<ImportWarning>,

    pub namespaces: HashSet<IdentPath>,
}

pub fn import_lib(path: &Path, type_ctx: Option<&mut Context>) -> ImportResult<ImportedLibrary> {
    let mut file = fs::File::open(path)?;

    let mut lib_bytes = Vec::new();
    file.read_to_end(&mut lib_bytes)?;

    let library = decode_lib(&lib_bytes)
        .map_err(|err| {
            let msg = format!("failed to decode {}: {err}", path.display());
            ImportError::IOError(io::Error::new(err.kind(), msg))
        })?;

    let mut builder = ImportBuilder::new(&library, type_ctx);

    builder.import()?;

    Ok(ImportedLibrary {
        namespaces: builder.namespaces,
        warnings: builder.warnings,
        imported_funcs: builder.imported_funcs,
        library,
    })
}
