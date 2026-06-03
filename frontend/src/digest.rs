mod digest_function;
mod digest_type;
mod error;
mod digest_builder;

pub use self::error::*;
use crate::ast::IdentPath;
use crate::codegen::library_builder::FunctionDeclKey;
use crate::codegen::FunctionInstance;
use crate::digest::digest_builder::DigestBuilder;
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
pub struct DigestOutput {
    pub library: ir::Library,
    pub imported_funcs: HashMap<FunctionDeclKey, FunctionInstance>,

    pub warnings: Vec<DigestWarning>,

    pub namespaces: HashSet<IdentPath>,
}

pub fn digest(path: &Path, type_ctx: Option<&mut Context>) -> DigestResult<DigestOutput> {
    let mut file = fs::File::open(path)?;

    let mut lib_bytes = Vec::new();
    file.read_to_end(&mut lib_bytes)?;

    let library = decode_lib(&lib_bytes)
        .map_err(|err| {
            let msg = format!("failed to decode {}: {err}", path.display());
            DigestError::IOError(io::Error::new(err.kind(), msg))
        })?;

    let mut builder = DigestBuilder::new(&library, type_ctx);

    builder.digest()?;

    Ok(DigestOutput {
        namespaces: builder.namespaces,
        warnings: builder.warnings,
        imported_funcs: builder.imported_funcs,
        library,
    })
}
