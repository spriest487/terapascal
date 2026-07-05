use terapascal_common::ident::IdentPath;
use crate::typ::Type;
use crate::IntConstant;
use derivative::Derivative;
use std::fmt;

pub const MAX_FLAGS_BITS: usize = 256;

#[derive(Derivative, Clone, Eq)]
#[derivative(Debug, PartialEq, Hash)]
pub struct SetDef {
    pub name: Option<IdentPath>,
    pub item_type: Type,

    pub min: IntConstant,
    pub max: IntConstant,
}

impl SetDef {
    pub fn flags_type_bits(&self) -> usize {
        let range = self.max.as_i128() - self.min.as_i128();
        if range <= 64 {
            64
        } else if range <= 128 {
            128
        } else {
            MAX_FLAGS_BITS
        }
    }
}

impl fmt::Display for SetDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}", name)
        } else {
            write!(f, "set of [{}..{}]", self.min, self.max)
        }
    }
}
