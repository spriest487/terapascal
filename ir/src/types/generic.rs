use crate::Type;
use serde::Deserialize;
use serde::Serialize;
use std::fmt;
use std::sync::Arc;
use terapascal_common::write_joined;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct TypeParam {
    pub name: Arc<String>,
    pub constraint: Option<Type>,
}

pub fn format_type_args(f: &mut fmt::Formatter, args: &[Type]) -> fmt::Result {
    if args.is_empty() {
        return Ok(());
    }

    write!(f, "[")?;
    write_joined(f, ", ", args.iter())?;
    write!(f, "]")
}