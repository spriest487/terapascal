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
use std::fmt;

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
        let mut buf = self.path.join(".");

        if !self.type_args.is_empty() {
            buf.push('[');
            for (i, ty_arg) in self.type_args.iter().enumerate() {
                if i > 0 {
                    buf.push_str(", ");
                }

                let ty_name = ty_arg.to_pretty_string(formatter);
                buf.push_str(&ty_name);
            }
            buf.push(']');
        }

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
