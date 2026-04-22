use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::typ;
use crate::{ast, ir};

pub trait NamePathExt {
    fn from_ident_path(ident: &ast::IdentPath, type_args: impl IntoIterator<Item=ir::Type>) -> Self;
    fn from_parts<Iter: IntoIterator<Item = String>>(iter: Iter) -> Self;
}

impl NamePathExt for ir::NamePath {
    fn from_ident_path(ident: &ast::IdentPath, type_args: impl IntoIterator<Item=ir::Type>) -> Self {
        let path = ident.iter()
            .map(|ident| ident.to_string())
            .collect();

        ir::NamePath { 
            path,
            type_args: type_args.into_iter().collect(),
        }
    }

    fn from_parts<Iter: IntoIterator<Item = String>>(iter: Iter) -> Self {
        ir::NamePath {
            path: iter.into_iter().collect(),
            type_args: Vec::new(),
        }
    }
}

pub fn translate_name(
    name: &typ::Symbol,
    lib: &mut LibraryBuilder,
) -> ir::NamePath {
    let path = ast::IdentPath::to_string_path(&name.full_path).into_vec();

    let type_args = name.type_args
        .as_ref()
            .map(|name_type_args_list| {
            name_type_args_list
                .items
                .iter()
                .map(|arg| lib.translate_type(arg))
                .collect()
        })
        .unwrap_or_else(Vec::new);

    ir::NamePath {
        path,
        type_args,
    }
}
