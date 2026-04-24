use crate::ir;
use crate::ptr::Pointer;
use cast::i128;
use std::ops::Index;
use std::ops::IndexMut;
use std::rc::Rc;
use crate::marshal::{MarshalResult, Marshaller, TypeIndex};

#[derive(Debug, Clone)]
pub enum DynValue {
    Bool(bool),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    ISize(isize),
    USize(usize),
    F32(f32),
    F64(f64),
    Function(ir::FunctionID),
    Structure(Box<StructValue>),
    Variant(Box<VariantValue>),
    Pointer(Pointer),
    Array(Box<ArrayValue>),
}

impl DynValue {
    pub fn value_type_category(&self) -> &'static str {
        match self {
            DynValue::Bool(_) => "bool",
            DynValue::I8(_) => "i8",
            DynValue::U8(_) => "u8",
            DynValue::I16(_) => "i16",
            DynValue::U16(_) => "u16",
            DynValue::I32(_) => "i32",
            DynValue::U32(_) => "u32",
            DynValue::I64(_) => "i64",
            DynValue::U64(_) => "u64",
            DynValue::ISize(_) => "isize",
            DynValue::USize(_) => "usize",
            DynValue::F32(_) => "f32",
            DynValue::F64(_) => "f64",
            DynValue::Function(_) => "function",
            DynValue::Structure(_) => "structure",
            DynValue::Variant(_) => "variant",
            DynValue::Pointer(_) => "pointer",
            DynValue::Array(_) => "array",
        }
    }

    pub fn nil(ty: ir::Type) -> Self {
        DynValue::Pointer(Pointer::nil(ty))
    }

    pub fn try_cast(&self, ty: &ir::Type, marshaller: &Marshaller) -> Option<Self> {
        match ty {
            ir::Type::Nothing => None,
            ir::Type::Generic(..) => None,

            ir::Type::I8 => self.to_bigint().map(|x| x as i8).map(DynValue::I8),
            ir::Type::U8 => self.to_bigint().map(|x| x as u8).map(DynValue::U8),
            ir::Type::I16 => self.to_bigint().map(|x| x as i16).map(DynValue::I16),
            ir::Type::U16 => self.to_bigint().map(|x| x as u16).map(DynValue::U16),
            ir::Type::I32 => self.to_bigint().map(|x| x as i32).map(DynValue::I32),
            ir::Type::U32 => self.to_bigint().map(|x| x as u32).map(DynValue::U32),
            ir::Type::I64 => self.to_bigint().map(|x| x as i64).map(DynValue::I64),
            ir::Type::U64 => self.to_bigint().map(|x| x as u64).map(DynValue::U64),
            ir::Type::ISize => self.to_bigint().map(|x| x as isize).map(DynValue::ISize),
            ir::Type::USize => self.to_bigint().map(|x| x as usize).map(DynValue::USize),
            ir::Type::Bool => self.to_bigint().map(|i| DynValue::Bool(i != 0)),

            ir::Type::F32 => match self {
                DynValue::F32(..) => Some(self.clone()),
                DynValue::F64(float) => Some(DynValue::F32(*float as f32)),

                _ => self.to_bigint().map(|x| x as f32).map(DynValue::F32),
            },

            ir::Type::F64 => match self {
                DynValue::F64(..) => Some(self.clone()),
                DynValue::F32(float) => Some(DynValue::F64(*float as f64)),

                _ => self.to_bigint().map(|x| x as f64).map(DynValue::F64),
            },

            ir::Type::TempRef(deref_ty)
            | ir::Type::Pointer(deref_ty) => {
                let addr = cast::usize(self.to_bigint()?).ok()?;
                let ptr = Pointer {
                    ty: (**deref_ty).clone(),
                    addr,
                };
                Some(DynValue::Pointer(ptr))
            },

            ir::Type::Object(..) | ir::Type::WeakObject(..) => {
                // assume self is a pointer-compatible type, its int value be the address
                let addr = cast::usize(self.to_bigint()?).ok()?;

                let ptr = Pointer {
                    addr,
                    // object types don't have a usable data layout behind the pointer, and the
                    // type can be discovered by reading the object header instead
                    ty: ir::Type::Nothing,
                };
                Some(DynValue::Pointer(ptr))
            },

            ir::Type::Struct { .. } => {
                let DynValue::Structure(s) = self else {
                    return None;
                };

                let struct_type_index = marshaller.try_get_type_index(ty)?;

                (s.type_index == struct_type_index).then(|| self.clone())
            },

            ir::Type::Variant(..) => {
                let DynValue::Variant(v) = self else {
                    return None;
                };

                let variant_type_index = marshaller.try_get_type_index(ty)?;

                (v.type_index == variant_type_index).then(|| self.clone())
            },

            ir::Type::Array { element, dim } => match self {
                DynValue::Array(arr) if arr.element_type == **element && arr.elements.len() == *dim => {
                    Some(self.clone())
                },
                _ => None,
            },

            ir::Type::Flags(..) => {
                let DynValue::Structure(s) = self else {
                    return None;
                };

                let flags_type_index = marshaller.get_type_index(ty).ok()?;
                (s.type_index == flags_type_index).then(|| self.clone())
            },

            ir::Type::Function(..) => None,
        }
    }

    pub fn try_eq(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (DynValue::Bool(a), DynValue::Bool(b)) => Some(a == b),
            (DynValue::I8(a), DynValue::I8(b)) => Some(a == b),
            (DynValue::U8(a), DynValue::U8(b)) => Some(a == b),
            (DynValue::I16(a), DynValue::I16(b)) => Some(a == b),
            (DynValue::U16(a), DynValue::U16(b)) => Some(a == b),
            (DynValue::I32(a), DynValue::I32(b)) => Some(a == b),
            (DynValue::U32(a), DynValue::U32(b)) => Some(a == b),
            (DynValue::I64(a), DynValue::I64(b)) => Some(a == b),
            (DynValue::U64(a), DynValue::U64(b)) => Some(a == b),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(a == b),
            (DynValue::USize(a), DynValue::USize(b)) => Some(a == b),

            (DynValue::F32(a), DynValue::F32(b)) => Some(a == b),
            (DynValue::F64(a), DynValue::F64(b)) => Some(a == b),

            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(a == b),
            (DynValue::Structure(a), DynValue::Structure(b)) => Some(a == b),
            (DynValue::Array(a), DynValue::Array(b)) => a.try_eq(b),
            _ => None,
        }
    }

    pub fn try_not(&self) -> Option<bool> {
        match self {
            DynValue::Bool(b) => Some(!*b),
            _ => None,
        }
    }

    pub fn try_add(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a.wrapping_add(*b))),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a.wrapping_add(*b))),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a.wrapping_add(*b))),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a.wrapping_add(*b))),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a.wrapping_add(*b))),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a.wrapping_add(*b))),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a.wrapping_add(*b))),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a.wrapping_add(*b))),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a.wrapping_add(*b))),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a.wrapping_add(*b))),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => {
                Some(DynValue::Pointer(a.addr_add(b.addr)))
            },
            (DynValue::Pointer(ptr), DynValue::ISize(offset)) => {
                Some(DynValue::Pointer(ptr.addr_add(*offset as usize)))
            },
            (DynValue::Pointer(ptr), DynValue::USize(offset)) => {
                Some(DynValue::Pointer(ptr.addr_add(*offset)))
            },

            (DynValue::F32(a), DynValue::F32(b)) => Some(DynValue::F32(a + b)),
            (DynValue::F64(a), DynValue::F64(b)) => Some(DynValue::F64(a + b)),

            _ => None,
        }
    }

    pub fn try_sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a.wrapping_sub(*b))),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a.wrapping_sub(*b))),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a.wrapping_sub(*b))),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a.wrapping_sub(*b))),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a.wrapping_sub(*b))),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a.wrapping_sub(*b))),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a.wrapping_sub(*b))),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a.wrapping_sub(*b))),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a.wrapping_sub(*b))),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a.wrapping_sub(*b))),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => {
                let ptr_diff = a.addr_sub(b.addr).addr as isize;
                Some(DynValue::ISize(ptr_diff))
            },
            (DynValue::Pointer(ptr), DynValue::ISize(offset)) => {
                Some(DynValue::Pointer(ptr.addr_sub(*offset as usize)))
            },
            (DynValue::Pointer(ptr), DynValue::USize(offset)) => {
                Some(DynValue::Pointer(ptr.addr_sub(*offset)))
            },

            (DynValue::F32(a), DynValue::F32(b)) => Some(DynValue::F32(a - b)),
            (DynValue::F64(a), DynValue::F64(b)) => Some(DynValue::F64(a - b)),

            _ => None,
        }
    }

    pub fn try_mul(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a.wrapping_mul(*b))),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a.wrapping_mul(*b))),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a.wrapping_mul(*b))),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a.wrapping_mul(*b))),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a.wrapping_mul(*b))),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a.wrapping_mul(*b))),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a.wrapping_mul(*b))),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a.wrapping_mul(*b))),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a.wrapping_mul(*b))),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a.wrapping_mul(*b))),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => {
                Some(DynValue::Pointer(a.addr_mul(b.addr)))
            },

            (DynValue::F32(a), DynValue::F32(b)) => Some(DynValue::F32(a * b)),
            (DynValue::F64(a), DynValue::F64(b)) => Some(DynValue::F64(a * b)),

            _ => None,
        }
    }

    pub fn try_idiv(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a.wrapping_div(*b))),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a.wrapping_div(*b))),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a.wrapping_div(*b))),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a.wrapping_div(*b))),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a.wrapping_div(*b))),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a.wrapping_div(*b))),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a.wrapping_div(*b))),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a.wrapping_div(*b))),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a.wrapping_div(*b))),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a.wrapping_div(*b))),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => {
                Some(DynValue::Pointer(a.addr_div(b.addr)))
            },

            (DynValue::F32(a), DynValue::F32(b)) => {
                let rounded = f32::round(a / b);
                Some(DynValue::F32(rounded))
            },
            (DynValue::F64(a), DynValue::F64(b)) => {
                let rounded = f64::round(a / b);
                Some(DynValue::F64(rounded))
            },

            _ => None,
        }
    }

    pub fn try_fdiv(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::F32(a), DynValue::F32(b)) => Some(DynValue::F32(a / b)),
            (DynValue::F64(a), DynValue::F64(b)) => Some(DynValue::F64(a / b)),

            _ => None,
        }
    }

    pub fn try_mod(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a.wrapping_rem(*b))),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a.wrapping_rem(*b))),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a.wrapping_rem(*b))),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a.wrapping_rem(*b))),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a.wrapping_rem(*b))),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a.wrapping_rem(*b))),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a.wrapping_rem(*b))),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a.wrapping_rem(*b))),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a.wrapping_rem(*b))),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a.wrapping_rem(*b))),

            (DynValue::F32(a), DynValue::F32(b)) => Some(DynValue::F32(a % b)),
            (DynValue::F64(a), DynValue::F64(b)) => Some(DynValue::F64(a % b)),

            _ => None,
        }
    }

    pub fn try_shl(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a << b)),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a << b)),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a << b)),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a << b)),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a << b)),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a << b)),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a << b)),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a << b)),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a << b)),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a << b)),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => {
                Some(DynValue::Pointer(a.addr_shl(b.addr)))
            },

            _ => None,
        }
    }

    pub fn try_shr(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(DynValue::I8(a >> b)),
            (DynValue::U8(a), DynValue::U8(b)) => Some(DynValue::U8(a >> b)),
            (DynValue::I16(a), DynValue::I16(b)) => Some(DynValue::I16(a >> b)),
            (DynValue::U16(a), DynValue::U16(b)) => Some(DynValue::U16(a >> b)),
            (DynValue::I32(a), DynValue::I32(b)) => Some(DynValue::I32(a >> b)),
            (DynValue::U32(a), DynValue::U32(b)) => Some(DynValue::U32(a >> b)),
            (DynValue::I64(a), DynValue::I64(b)) => Some(DynValue::I64(a >> b)),
            (DynValue::U64(a), DynValue::U64(b)) => Some(DynValue::U64(a >> b)),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(DynValue::ISize(a >> b)),
            (DynValue::USize(a), DynValue::USize(b)) => Some(DynValue::USize(a >> b)),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => {
                Some(DynValue::Pointer(a.addr_shr(b.addr)))
            },

            _ => None,
        }
    }

    pub fn try_gt(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(a > b),
            (DynValue::U8(a), DynValue::U8(b)) => Some(a > b),
            (DynValue::I16(a), DynValue::I16(b)) => Some(a > b),
            (DynValue::U16(a), DynValue::U16(b)) => Some(a > b),
            (DynValue::I32(a), DynValue::I32(b)) => Some(a > b),
            (DynValue::U32(a), DynValue::U32(b)) => Some(a > b),
            (DynValue::I64(a), DynValue::I64(b)) => Some(a > b),
            (DynValue::U64(a), DynValue::U64(b)) => Some(a > b),
            (DynValue::F32(a), DynValue::F32(b)) => Some(a > b),
            (DynValue::F64(a), DynValue::F64(b)) => Some(a > b),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(a > b),
            (DynValue::USize(a), DynValue::USize(b)) => Some(a > b),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(a.addr > b.addr),

            _ => None,
        }
    }

    pub fn try_gte(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(a >= b),
            (DynValue::U8(a), DynValue::U8(b)) => Some(a >= b),
            (DynValue::I16(a), DynValue::I16(b)) => Some(a >= b),
            (DynValue::U16(a), DynValue::U16(b)) => Some(a >= b),
            (DynValue::I32(a), DynValue::I32(b)) => Some(a >= b),
            (DynValue::U32(a), DynValue::U32(b)) => Some(a >= b),
            (DynValue::I64(a), DynValue::I64(b)) => Some(a >= b),
            (DynValue::U64(a), DynValue::U64(b)) => Some(a >= b),
            (DynValue::F32(a), DynValue::F32(b)) => Some(a >= b),
            (DynValue::F64(a), DynValue::F64(b)) => Some(a >= b),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(a >= b),
            (DynValue::USize(a), DynValue::USize(b)) => Some(a >= b),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(a.addr >= b.addr),

            _ => None,
        }
    }

    pub fn try_lt(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(a < b),
            (DynValue::U8(a), DynValue::U8(b)) => Some(a < b),
            (DynValue::I16(a), DynValue::I16(b)) => Some(a < b),
            (DynValue::U16(a), DynValue::U16(b)) => Some(a < b),
            (DynValue::I32(a), DynValue::I32(b)) => Some(a < b),
            (DynValue::U32(a), DynValue::U32(b)) => Some(a < b),
            (DynValue::I64(a), DynValue::I64(b)) => Some(a < b),
            (DynValue::U64(a), DynValue::U64(b)) => Some(a < b),
            (DynValue::F32(a), DynValue::F32(b)) => Some(a < b),
            (DynValue::F64(a), DynValue::F64(b)) => Some(a < b),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(a < b),
            (DynValue::USize(a), DynValue::USize(b)) => Some(a < b),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(a.addr < b.addr),

            _ => None,
        }
    }

    pub fn try_lte(&self, other: &Self) -> Option<bool> {
        match (self, other) {
            (DynValue::I8(a), DynValue::I8(b)) => Some(a <= b),
            (DynValue::U8(a), DynValue::U8(b)) => Some(a <= b),
            (DynValue::I16(a), DynValue::I16(b)) => Some(a <= b),
            (DynValue::U16(a), DynValue::U16(b)) => Some(a <= b),
            (DynValue::I32(a), DynValue::I32(b)) => Some(a <= b),
            (DynValue::U32(a), DynValue::U32(b)) => Some(a <= b),
            (DynValue::I64(a), DynValue::I64(b)) => Some(a <= b),
            (DynValue::U64(a), DynValue::U64(b)) => Some(a <= b),
            (DynValue::F32(a), DynValue::F32(b)) => Some(a <= b),
            (DynValue::F64(a), DynValue::F64(b)) => Some(a <= b),
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(a <= b),
            (DynValue::USize(a), DynValue::USize(b)) => Some(a <= b),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => Some(a.addr <= b.addr),

            _ => None,
        }
    }

    pub fn as_function(&self) -> Option<ir::FunctionID> {
        match self {
            DynValue::Function(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_struct_mut(&mut self, type_index: TypeIndex) -> Option<&mut StructValue> {
        match self {
            DynValue::Structure(struct_val) if type_index == struct_val.type_index => {
                Some(struct_val)
            },
            _ => None,
        }
    }

    pub fn as_struct(&self, type_index: TypeIndex) -> Option<&StructValue> {
        match self {
            DynValue::Structure(struct_val) if type_index == struct_val.type_index => {
                Some(struct_val)
            },
            _ => None,
        }
    }

    pub fn as_any_struct(&self) -> Option<&StructValue> {
        match self {
            DynValue::Structure(struct_val) => Some(struct_val),
            _ => None,
        }
    }

    pub fn as_array(&self, el_ty: &ir::Type) -> Option<&[DynValue]> {
        match self {
            DynValue::Array(arr) if arr.element_type == *el_ty => Some(&arr.elements),
            _ => None,
        }
    }

    pub fn as_variant(&self, type_index: TypeIndex) -> Option<&VariantValue> {
        match self {
            DynValue::Variant(var_val) if type_index == var_val.type_index => Some(var_val),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            DynValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_i8(&self) -> Option<i8> {
        match self {
            DynValue::I8(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_u8(&self) -> Option<u8> {
        match self {
            DynValue::U8(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_i16(&self) -> Option<i16> {
        match self {
            DynValue::I16(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_u16(&self) -> Option<u16> {
        match self {
            DynValue::U16(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_i32(&self) -> Option<i32> {
        match self {
            DynValue::I32(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_u32(&self) -> Option<u32> {
        match self {
            DynValue::U32(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            DynValue::I64(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_u64(&self) -> Option<u64> {
        match self {
            DynValue::U64(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_isize(&self) -> Option<isize> {
        match self {
            DynValue::ISize(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_usize(&self) -> Option<usize> {
        match self {
            DynValue::USize(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_f32(&self) -> Option<f32> {
        match self {
            DynValue::F32(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            DynValue::F64(f) => Some(*f),
            _ => None,
        }
    }

    // might need to replace i128 with an actual bigint type at one point, this is valid as long
    // as i128 can hold any representable integer type in the language
    pub fn to_bigint(&self) -> Option<i128> {
        match self {
            DynValue::I8(x) => Some(*x as i128),
            DynValue::U8(x) => Some(*x as i128),
            DynValue::I16(x) => Some(*x as i128),
            DynValue::U16(x) => Some(*x as i128),
            DynValue::I32(x) => Some(*x as i128),
            DynValue::U32(x) => Some(*x as i128),
            DynValue::I64(x) => Some(*x as i128),
            DynValue::U64(x) => Some(*x as i128),
            DynValue::ISize(x) => Some(*x as i128),
            DynValue::USize(x) => Some(*x as i128),
            DynValue::Pointer(ptr) => Some(ptr.addr as i128),
            DynValue::Bool(true) => Some(1),
            DynValue::Bool(false) => Some(0),
            DynValue::F32(f) => i128(*f).ok(),
            DynValue::F64(f) => i128(*f).ok(),
            _ => None,
        }
    }

    pub fn as_pointer(&self) -> Option<&Pointer> {
        match self {
            DynValue::Pointer(ptr) => Some(ptr),
            _ => None,
        }
    }
}

impl From<StructValue> for DynValue {
    fn from(s: StructValue) -> Self {
        DynValue::Structure(Box::new(s))
    }
}

impl From<ArrayValue> for DynValue {
    fn from(arr: ArrayValue) -> Self {
        DynValue::Array(Box::new(arr))
    }
}

impl From<Pointer> for DynValue {
    fn from(ptr: Pointer) -> Self {
        DynValue::Pointer(ptr)
    }
}

#[derive(Debug, Clone)]
pub struct StructValue {
    pub type_index: TypeIndex,
    pub fields: Vec<DynValue>,
}

impl StructValue {
    pub fn new(type_index: TypeIndex, fields: impl IntoIterator<Item = DynValue>) -> Self {
        Self {
            type_index,
            fields: fields.into_iter().collect(),
        }
    }
}

impl Index<ir::FieldID> for StructValue {
    type Output = DynValue;

    fn index(&self, index: ir::FieldID) -> &DynValue {
        &self.fields[index.0]
    }
}

impl IndexMut<ir::FieldID> for StructValue {
    fn index_mut(&mut self, index: ir::FieldID) -> &mut DynValue {
        &mut self.fields[index.0]
    }
}

impl PartialEq<Self> for StructValue {
    fn eq(&self, other: &Self) -> bool {
        if self.type_index != other.type_index || self.fields.len() != other.fields.len() {
            return false;
        }

        self.fields
            .iter()
            .zip(other.fields.iter())
            .all(|(a, b)| match a.try_eq(b) {
                Some(eq) => eq,
                None => panic!("structs can only contain comparable fields"),
            })
    }
}

// subset of ir::ObjectID that only includes concrete types
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum ObjectID {
    Struct(TypeIndex),
    Array(Rc<ir::Type>),
    Box(Rc<ir::Type>),
}

impl ObjectID {
    pub fn try_from_type(ty: &ir::Type, marshaller: &Marshaller) -> Option<Self> {
        match ty {
            ir::Type::Object(id) | ir::Type::WeakObject(id) => {
                match id {
                    // real types
                    ir::ObjectID::Class(..) => {
                        let type_index = marshaller.try_get_type_index(ty)?;
                        Some(ObjectID::Struct(type_index))
                    },
                    ir::ObjectID::Array(element_type) => Some(ObjectID::Array(element_type.clone())),
                    ir::ObjectID::Box(value_type) => Some(ObjectID::Box(value_type.clone())),

                    // abstract types
                    ir::ObjectID::Any
                    | ir::ObjectID::Interface(_)
                    | ir::ObjectID::AnyClosure(_) => None,
                }
            },

            _ => None,
        }
    }
    
    pub fn to_type(&self, marshaller: &Marshaller) -> MarshalResult<ir::Type> {
        match self {
            ObjectID::Struct(id) => {
                marshaller.get_type(*id).cloned()
            },
            ObjectID::Array(element) => {
                Ok(ir::ObjectID::Array(element.clone()).to_object_type())
            },
            ObjectID::Box(value) => {
                Ok(ir::ObjectID::Box(value.clone()).to_object_type())
            },
        }
    }

    pub(crate) fn to_pretty_name(&self, marshaller: &Marshaller) -> String {
        self.to_type(marshaller)
            .map(|t| t.to_pretty_string(marshaller.metadata()))
            .unwrap_or_else(|_| format!("{:?}", self))
    }
}

#[derive(Debug, Clone)]
pub struct ObjectHeader {
    pub id: ObjectID,
    
    pub strong_count: i32,
    pub weak_count: i32,
}

impl ObjectHeader {
    pub fn immortal(id: ObjectID) -> Self {
        Self {
            id,

            strong_count: -1,
            weak_count: 0,
        }
    }

    pub fn new(id: ObjectID, immortal: bool) -> Self {
        Self {
            id,
            
            strong_count: if immortal { -1 } else { 1 },
            weak_count: 0,
        }
    }

    pub fn is_immortal(&self) -> bool {
        self.strong_count < 0
    }
}

#[derive(Debug, Clone)]
pub struct ObjectValue {
    pub header: ObjectHeader,
    pub value: DynValue,
}

#[derive(Debug, Clone)]
pub struct VariantValue {
    pub type_index: TypeIndex,
    pub tag: Box<DynValue>,
    pub data: Box<DynValue>,
}

#[derive(Debug, Clone)]
pub struct ArrayValue {
    pub element_type: ir::Type,
    pub elements: Vec<DynValue>,
}

impl ArrayValue {
    pub fn try_eq(&self, other: &Self) -> Option<bool> {
        if self.elements.len() == other.elements.len() {
            let mut all_same = true;
            for (mine, theirs) in self.elements.iter().zip(other.elements.iter()) {
                all_same &= mine.try_eq(theirs)?;
            }
            Some(all_same)
        } else {
            None
        }
    }

    pub fn array_ty(&self) -> ir::Type {
        self.element_type.clone().array(self.elements.len())
    }
}
