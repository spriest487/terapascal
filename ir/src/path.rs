use std::fmt;
use std::iter;
use std::sync::Arc;
use serde::Deserialize;
use serde::Serialize;
use terapascal_common::path::Path;
use crate::Type;
use crate::RawFormatter;
use crate::IRFormatter;
use crate::TypeParam;

pub type StringPath = Path<Arc<String>>;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct DeclPath {
    pub path: StringPath,
    pub type_params: Vec<TypeParam>,
}

impl DeclPath {
    pub fn new<P>(namespace: impl IntoIterator<Item=P>, name: impl Into<Arc<String>>) -> Self
    where
        P: Into<Arc<String>>,
    {
        let namespace_parts = namespace
            .into_iter()
            .map(|part| part.into());

        Self {
            path: Path::from_parts(namespace_parts.chain(iter::once(name.into()))),
            type_params: Vec::new(),
        }
    }

    pub fn with_type_params(mut self, type_params: impl IntoIterator<Item=TypeParam>) -> Self {
        self.type_params = type_params.into_iter().collect();
        self
    }

    pub fn name(&self) -> &Arc<String> {
        self.path.last()
    }

    pub fn name_mut (&mut self) -> &mut Arc<String> {
        self.path.last_mut()
    }

    pub fn to_pretty_string(&self, formatter: &impl IRFormatter) -> String {
        let mut buf = String::new();
        _ = formatter.format_decl(self, &mut buf);
        buf
    }

    pub fn parent(&self) -> Option<StringPath> {
        if self.path.len() < 2 {
            return None;
        }

        let mut path = self.path.clone();
        path.pop();

        Some(path)
    }

    pub fn child(mut self, name: impl Into<Arc<String>>) -> Self {
        self.path.push(name.into());
        self
    }

    pub fn generic_args(&self) -> Vec<Type> {
        self.type_params
            .iter()
            .map(|p| Type::Generic(p.name.clone()))
            .collect()
    }

    pub fn to_generic_name(&self) -> NamePath {
        NamePath {
            path: self.path.clone(),
            type_args: self.generic_args(),
        }
    }
}

impl fmt::Display for DeclPath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        RawFormatter.format_decl(self, f)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct NamePath {
    pub path: StringPath,
    pub type_args: Vec<Type>,
}

impl NamePath {
    pub fn new<P>(namespace: impl IntoIterator<Item=P>, name: impl Into<Arc<String>>) -> Self
    where
        P: Into<Arc<String>>,
    {
        let ns_parts = namespace
            .into_iter()
            .map(|p| p.into());

        NamePath {
            path: Path::from_parts(ns_parts.chain(iter::once(name.into()))),
            type_args: Vec::new(),
        }
    }

    pub fn with_ty_args(self, args: impl IntoIterator<Item = Type>) -> Self {
        assert!(
            self.type_args.is_empty(),
            "with_type_args: name must not already have a type argument list"
        );

        Self {
            path: self.path,
            type_args: args.into_iter().collect(),
        }
    }

    pub fn name(&self) -> &Arc<String> {
        self.path.last()
    }

    pub fn name_mut (&mut self) -> &mut Arc<String> {
        self.path.last_mut()
    }

    pub fn to_pretty_string(&self, formatter: &impl IRFormatter) -> String {
        let mut buf = String::new();
        _ = formatter.format_name(self, &mut buf);
        buf
    }

    pub fn parent(&self) -> Option<Self> {
        if self.path.len() < 2 {
            return None;
        }

        let mut path = self.path.clone();
        path.pop();

        Some(Self {
            path,
            type_args: Vec::new()
        })
    }

    pub fn child(self, name: impl Into<Arc<String>>) -> Self {
        let path = self.path.child(name.into());
        Self {
            path,
            type_args: Vec::new(),
        }
    }

    pub fn is_generic(&self) -> bool {
        if self.type_args.is_empty() {
            return false;
        }

        self.type_args.iter().all(|ty| {
            ty.as_generic_param().is_some()
        })
    }
}

impl fmt::Display for NamePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        RawFormatter.format_name(self, f)
    }
}