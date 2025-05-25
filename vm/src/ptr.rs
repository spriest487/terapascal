use crate::dyn_value::DynValue;
use crate::ir;
use crate::ExecResult;
use crate::Interpreter;
use std::cmp::Ordering;
use std::fmt;
use std::mem::size_of;

pub const POINTER_FMT_WIDTH: usize = size_of::<usize>() * 2;

/// pointer to native memory that is marshalled to/from value cells when accessed
#[derive(Debug, Clone, Eq)]
pub struct Pointer {
    pub addr: usize,
    pub ty: ir::Type,
}

impl fmt::Display for Pointer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:0width$x} ({})", self.addr, self.ty, width = POINTER_FMT_WIDTH)
    }
}

impl Pointer {
    pub fn nil(ty: ir::Type) -> Self {
        Self {
            addr: 0,
            ty
        }
    }

    pub fn is_null(&self) -> bool {
        self.addr == 0
    }

    pub fn deref_ptr(&self, state: &Interpreter) -> ExecResult<DynValue> {
        state.load_indirect(self)
    }

    pub fn reinterpret(&self, ty: ir::Type) -> Self {
        Self {
            addr: self.addr,
            ty,
        }
    }

    pub fn addr_add(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr.wrapping_add(rhs) }
    }

    pub fn addr_sub(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr.wrapping_sub(rhs) }
    }

    pub fn addr_mul(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr.wrapping_mul(rhs) }
    }

    pub fn addr_div(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr.wrapping_div(rhs) }
    }

    pub fn addr_shl(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr << rhs }
    }

    pub fn addr_shr(&self, rhs: usize) -> Self {
        Self { ty: self.ty.clone(), addr: self.addr >> rhs }
    }

    pub fn to_pretty_string(&self, metadata: &ir::Metadata) -> String {
        let ty_pretty_name = metadata.pretty_ty_name(&self.ty);

        format!("0x{:0width$x} ({})", self.addr, ty_pretty_name, width = size_of::<usize>() * 2)
    }
}

impl PartialOrd for Pointer {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pointer {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (a, b) => a.addr.cmp(&b.addr),
        }
    }
}

impl PartialEq for Pointer {
    fn eq(&self, other: &Self) -> bool {
        self.addr == other.addr 
    }
}
