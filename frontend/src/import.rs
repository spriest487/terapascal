mod error;
mod builder;

pub use self::error::*;

use terapascal_common::ident::IdentPath;
use crate::codegen::library_builder::FunctionDeclKey;
use crate::codegen::FunctionInstance;
use crate::import::builder::ImportBuilder;
use crate::ir;
use crate::typ::Context;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug)]
pub struct ImportOutput {
    pub imported_funcs: HashMap<FunctionDeclKey, FunctionInstance>,

    pub warnings: Vec<ImportWarning>,

    pub namespaces: HashSet<IdentPath>,
}

pub fn import_lib<'a>(
    library: &'a ir::Library,
    ref_libs: impl IntoIterator<Item=&'a ir::Library>,
    type_ctx: Option<&'a mut Context>,
) -> ImportResult<ImportOutput> {
    let mut builder = ImportBuilder::new(&library, ref_libs, type_ctx);

    builder.import()?;

    Ok(ImportOutput {
        namespaces: builder.imported_namespaces,
        warnings: builder.warnings,
        imported_funcs: builder.imported_funcs,
    })
}
