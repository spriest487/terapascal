use crate::codegen::library_builder::LibraryBuilder;
use crate::codegen::typ;
use crate::ir;
use std::sync::Arc;
use terapascal_common::ident::IdentPath;

pub trait PathExt<T> {
    fn from_ident_path(ident: &IdentPath, type_args: impl IntoIterator<Item=T>) -> Self;

    fn from_parts<P, Iter>(iter: Iter) -> Self
    where
        P: Into<Arc<String>>,
        Iter: IntoIterator<Item=P>;
}

impl PathExt<ir::Type> for ir::NamePath {
    fn from_ident_path(ident: &IdentPath, type_args: impl IntoIterator<Item=ir::Type>) -> Self {
        let path = ident.iter()
            .map(|ident| ident.name.clone())
            .collect();

        ir::NamePath { 
            path,
            type_args: type_args.into_iter().collect(),
        }
    }

    fn from_parts<P, Iter>(iter: Iter) -> Self
    where
        P: Into<Arc<String>>,
        Iter: IntoIterator<Item = P>,
    {
        ir::NamePath {
            path: iter.into_iter().map(|p| p.into()).collect(),
            type_args: Vec::new(),
        }
    }
}

pub fn translate_name(
    name: &typ::Symbol,
    lib: &mut LibraryBuilder,
) -> ir::NamePath {
    let path = IdentPath::map(&name.full_path, |p| p.name.clone());

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

impl PathExt<ir::TypeParam> for ir::DeclPath {
    fn from_ident_path(ident: &IdentPath, type_args: impl IntoIterator<Item=ir::TypeParam>) -> Self {
        Self {
            path: ident.map(|p| p.name.clone()),
            type_params: type_args.into_iter().collect(),
        }
    }

    fn from_parts<P, Iter>(iter: Iter) -> Self
    where
        P: Into<Arc<String>>,
        Iter: IntoIterator<Item=P>,
    {
        Self {
            path: iter.into_iter().map(|p| p.into()).collect(),
            type_params: Vec::new(),
        }
    }
}

pub fn translate_decl_name(
    name: &typ::Symbol,
    lib: &mut LibraryBuilder,
) -> ir::DeclPath {
    let path = IdentPath::map(&name.full_path, |p| p.name.clone());
    let type_params = lib.translate_type_param_list(name.type_params.as_ref());

    ir::DeclPath {
        path,
        type_params,
    }
}