pub mod dep_sort;
pub mod builtin;

mod typeinfo;
mod path;
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
pub use self::path::*;
pub use self::type_decl::*;
pub use self::typeinfo::*;
pub use self::types::*;
pub use self::val::*;

use serde::Deserialize;
use serde::Serialize;
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub enum Visibility {
    Internal,
    Public,
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
