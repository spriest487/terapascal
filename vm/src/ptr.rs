use crate::dyn_value::DynValue;
use crate::ir;
use crate::ExecResult;
use crate::Interpreter;
use std::cmp::Ordering;
use std::fmt;
use std::mem::size_of;
use std::ptr::{slice_from_raw_parts, slice_from_raw_parts_mut};

pub const POINTER_FMT_WIDTH: usize = size_of::<usize>() * 2;

/// pointer to native memory that is marshalled to/from value cells when accessed
#[derive(Clone, Eq)]
pub struct Pointer {
    pub addr: usize,
    pub ty: ir::Type,
}

impl fmt::Debug for Pointer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Pointer({} @ 0x{:0width$X})", self.ty, self.addr, width = POINTER_FMT_WIDTH)
    }
}

impl fmt::Display for Pointer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "0x{:0width$X} ({})", self.addr, self.ty, width = POINTER_FMT_WIDTH)
    }
}

impl Pointer {
    pub fn new(addr: usize, ty: ir::Type) -> Self {
        Pointer {
            addr,
            ty,
        }
    }
    
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
        let width = size_of::<usize>() * 2;

        if self.ty != ir::Type::Nothing {
            let ty_pretty_name = metadata.pretty_ty_name(&self.ty);

            format!("0x{:0WIDTH$x} ({})", self.addr, ty_pretty_name, WIDTH = width)
        } else {
            format!("0x{:0WIDTH$x}", self.addr, WIDTH = width)
        }
    }
    
    pub unsafe fn as_slice(&self, len: usize) -> &[u8] {
        unsafe {
            slice_from_raw_parts(self.addr as *const u8, len)
                .as_ref()
                .expect("as_slice: pointer must not be null")
        }
    }

    pub unsafe fn as_slice_mut(&self, len: usize) -> &mut [u8] {
        unsafe {
            slice_from_raw_parts_mut(self.addr as *mut u8, len)
                .as_mut()
                .expect("as_slice: pointer must not be null")
        }
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
