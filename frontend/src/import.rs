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

#[derive(Debug)]
pub struct ImportedLibrary {
    pub library: ir::Library,

    pub imported_funcs: HashMap<FunctionDeclKey, FunctionInstance>,

    pub warnings: Vec<ImportWarning>,

    pub namespaces: HashSet<IdentPath>,
}

pub fn import_lib(library: ir::Library, ref_libs: &[ImportedLibrary], type_ctx: Option<&mut Context>) -> ImportResult<ImportedLibrary> {
    let mut builder = ImportBuilder::new(&library, ref_libs, type_ctx);

    builder.import()?;

    Ok(ImportedLibrary {
        namespaces: builder.namespaces,
        warnings: builder.warnings,
        imported_funcs: builder.imported_funcs,
        library,
    })
}
