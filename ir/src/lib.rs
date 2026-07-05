pub mod dep_sort;
pub mod typeinfo;
pub mod builtin;

mod formatter;
mod function;
mod instruction;
mod metadata;
mod types;
mod type_decl;
mod val;
mod library;
mod instruction_builder;
mod error;

pub use self::error::MetadataError;
pub use self::formatter::*;
pub use self::function::*;
pub use self::instruction::*;
pub use self::instruction_builder::generic;
pub use self::instruction_builder::scope::*;
pub use self::instruction_builder::util;
pub use self::instruction_builder::InstructionBuilder;
pub use self::instruction_builder::LocalBinding;
pub use self::instruction_builder::RawInstructionBuilder;
pub use self::library::*;
pub use self::metadata::*;
pub use self::types::*;
pub use self::type_decl::*;
pub use self::typeinfo::*;
pub use self::val::*;

use serde::Deserialize;
use serde::Serialize;
use std::{fmt, iter};
use std::sync::Arc;
use terapascal_common::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct DeclPath {
    pub path: Path<Arc<String>>,
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
        &self.path.as_slice()[self.path.len() - 1]
    }

    pub fn name_mut (&mut self) -> &mut Arc<String> {
        let index = self.path.len() - 1;
        &mut self.path.as_mut_slice()[index]
    }

    pub fn to_pretty_string(&self, formatter: &impl IRFormatter) -> String {
        let mut buf = String::new();
        _ = formatter.format_decl(self, &mut buf);
        buf
    }

    pub fn parent(&self) -> Option<Path<Arc<String>>> {
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
}

impl fmt::Display for DeclPath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        RawFormatter.format_decl(self, f)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct NamePath {
    pub path: Vec<String>,
    pub type_args: Vec<Type>,
}

impl NamePath {
    pub fn new(ns: impl IntoIterator<Item = String>, name: impl Into<String>) -> Self {
        let mut path: Vec<_> = ns.into_iter().collect();
        path.push(name.into());

        NamePath {
            path,
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
    
    pub fn name(&self) -> &String {
        &self.path[self.path.len() - 1]
    }

    pub fn name_mut (&mut self) -> &mut String {
        let index = self.path.len() - 1;
        &mut self.path[index]
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

    pub fn child(mut self, name: impl Into<String>) -> Self {
        self.path.push(name.into());
        self
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

pub fn write_instruction_list(
    f: &mut dyn fmt::Write,
    metadata: &impl IRFormatter,
    instructions: &[Instruction],
) -> fmt::Result {
    let num_len = instructions.len().to_string().len();

    for (i, instruction) in instructions.iter().enumerate() {
        write!(f, "{:>width$}|", i, width = num_len)?;
        metadata.format_instruction(instruction, f)?;
        writeln!(f)?;
    }

    Ok(())
}
