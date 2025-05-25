mod formatter;
mod function;
mod instruction;
mod metadata;
mod ty;
mod ty_decl;
mod val;
mod library;
pub mod dep_sort;
pub mod rtti;

pub use formatter::*;
pub use function::*;
pub use instruction::*;
pub use library::*;
pub use metadata::*;
pub use rtti::*;
use serde::Deserialize;
use serde::Serialize;
use std::borrow::Cow;
use std::fmt;
pub use ty::*;
pub use ty_decl::*;
pub use val::*;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct NamePath {
    pub path: Vec<String>,
    pub type_args: Option<Vec<Type>>,
}

impl NamePath {
    pub fn new(ns: impl IntoIterator<Item = String>, name: impl Into<String>) -> Self {
        let mut path: Vec<_> = ns.into_iter().collect();
        path.push(name.into());

        NamePath {
            path,
            type_args: None,
        }
    }

    pub fn with_ty_args(self, args: impl IntoIterator<Item = Type>) -> Self {
        assert!(
            self.type_args.is_none(),
            "with_type_args: name must not already have a type argument list"
        );

        Self {
            path: self.path,
            type_args: Some(args.into_iter().collect()),
        }
    }

    pub fn to_pretty_string<'a, TyFormat>(&self, ty_format: TyFormat) -> String
    where TyFormat: Fn(&Type) -> Cow<'a, str>,
    {
        let mut buf = self.path.join(".");

        if let Some(type_args) = self.type_args.as_ref() {
            buf.push('[');
            for (i, ty_arg) in type_args.iter().enumerate() {
                if i > 0 {
                    buf.push_str(", ");
                }

                let ty_name = ty_format(ty_arg);
                buf.push_str(&ty_name);
            }
            buf.push(']');
        }

        buf
    }
}

impl fmt::Display for NamePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        RawInstructionFormatter.format_name(self, f)
    }
}

pub fn write_instruction_list(
    f: &mut fmt::Formatter,
    metadata: &Metadata,
    instructions: &[Instruction],
) -> fmt::Result {
    let num_len = instructions.len().to_string().len();

    let formatter = StatefulIndentedFormatter::new(metadata, 4);

    for (i, instruction) in instructions.iter().enumerate() {
        write!(f, "{:>width$}|", i, width = num_len)?;
        formatter.format_instruction(instruction, f)?;
        writeln!(f)?;
    }

    Ok(())
}
