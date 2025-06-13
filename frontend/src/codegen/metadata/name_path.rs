use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::typ;
use crate::{ast, ir};
use typ::Specializable;

pub trait NamePathExt {
    fn from_decl(name: typ::Symbol, metadata: &mut LibraryBuilder) -> Self;
    fn from_ident_path(ident: &ast::IdentPath, type_args: Option<Vec<ir::Type>>) -> Self;
    fn from_parts<Iter: IntoIterator<Item = String>>(iter: Iter) -> Self;
}

impl NamePathExt for ir::NamePath {
    fn from_decl(name: typ::Symbol, builder: &mut LibraryBuilder) -> Self {
        let path_parts = name
            .full_path
            .into_iter()
            .map(|ident| ident.to_string());

        let type_args = match name.type_args {
            Some(name_type_args) => {
                let types = name_type_args
                    .items
                    .iter()
                    .map(|arg| builder.find_type(arg))
                    .collect();

                Some(types)
            },

            None => None,
        };

        ir::NamePath {
            path: path_parts.collect(),
            type_args,
        }
    }

    fn from_ident_path(ident: &ast::IdentPath, type_args: Option<Vec<ir::Type>>) -> Self {
        let path = ident.iter()
            .map(|ident| ident.to_string())
            .collect();

        ir::NamePath { path, type_args }
    }

    fn from_parts<Iter: IntoIterator<Item = String>>(iter: Iter) -> Self {
        ir::NamePath {
            path: iter.into_iter().collect(),
            type_args: None,
        }
    }
}

pub fn translate_name(
    name: &typ::Symbol,
    generic_ctx: &typ::GenericContext,
    lib: &mut LibraryBuilder,
) -> ir::NamePath {
    let name = name.clone().apply_type_args(generic_ctx, generic_ctx);

    if name.is_unspecialized_generic() {
        panic!("can't translate unspecialized generic name: {}", name);
    }

    if let Some(name_type_args) = name.type_args.as_ref() {
        if let Some(t) = name_type_args.items.iter().find(|t| t.is_generic_param()) {
            panic!(
                "can't translate name containing generic parameters (found {}): {}",
                t, name
            );
        }
    }

    let path = ast::IdentPath::to_string_path(&name.full_path).into_vec();

    let type_args = name.type_args
        .as_ref()
            .map(|name_type_args_list| {
            name_type_args_list
                .items
                .iter()
                .map(|arg| lib.translate_type(arg, generic_ctx))
                .collect()
        });

    ir::NamePath {
        path,
        type_args,
    }
}
