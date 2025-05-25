use crate::ir;
use crate::ptr::Pointer;
use cast::i128;
use std::ops::Index;
use std::ops::IndexMut;

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

    pub fn try_cast(&self, ty: &ir::Type) -> Option<Self> {
        match ty {
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

            ir::Type::F32 => {
                if let DynValue::F32(..) = self {
                    return Some(self.clone());
                }

                self.to_bigint().map(|x| x as f32).map(DynValue::F32)
            },

            ir::Type::Pointer(deref_ty) => {
                let addr = cast::usize(self.to_bigint()?).ok()?;
                let ptr = Pointer {
                    ty: (**deref_ty).clone(),
                    addr,
                };
                Some(DynValue::Pointer(ptr))
            },

            ir::Type::Nothing => None,

            ir::Type::RcPointer(class_id) | ir::Type::RcWeakPointer(class_id) => {
                // assume self is a pointer-compatible type, its int value be the address
                let addr = cast::usize(self.to_bigint()?).ok()?;

                let ptr = Pointer {
                    addr,
                    ty: match class_id {
                        ir::VirtualTypeID::Class(struct_id) => ir::Type::Struct(*struct_id),

                        // Any, interfaces and closure pointers only have virtual types so we
                        // can't include any type info about the concrete type here - it's up to
                        // the language to have the right info when it uses this pointer value
                        ir::VirtualTypeID::Any | ir::VirtualTypeID::Interface(..) | ir::VirtualTypeID::Closure(..) => {
                            ir::Type::Nothing
                        },
                    },
                };
                Some(DynValue::Pointer(ptr))
            },

            ir::Type::Struct(id) => match self {
                DynValue::Structure(s) if s.type_id == *id => Some(self.clone()),
                _ => None,
            },

            ir::Type::Variant(id) => match self {
                DynValue::Variant(v) if v.type_id == *id => Some(self.clone()),
                _ => None,
            },

            ir::Type::Array { element, dim } => match self {
                DynValue::Array(arr) if arr.el_ty == **element && arr.elements.len() == *dim => {
                    Some(self.clone())
                },
                _ => None,
            },

            ir::Type::Flags(repr_id, _set_id) => match self {
                DynValue::Structure(s) if s.type_id == *repr_id => Some(self.clone()),
                _ => None,
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

            _ => None,
        }
    }

    pub fn try_fdiv(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (DynValue::F32(a), DynValue::F32(b)) => Some(DynValue::F32(a / b)),

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
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(a > b),
            (DynValue::USize(a), DynValue::USize(b)) => Some(a > b),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => {
                Some(a.addr > b.addr)
            },

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
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(a >= b),
            (DynValue::USize(a), DynValue::USize(b)) => Some(a >= b),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => {
                Some(a.addr >= b.addr)
            },

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
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(a < b),
            (DynValue::USize(a), DynValue::USize(b)) => Some(a < b),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => {
                Some(a.addr < b.addr)
            },

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
            (DynValue::ISize(a), DynValue::ISize(b)) => Some(a <= b),
            (DynValue::USize(a), DynValue::USize(b)) => Some(a <= b),
            (DynValue::Pointer(a), DynValue::Pointer(b)) => {
                Some(a.addr <= b.addr)
            },

            _ => None,
        }
    }    

    pub fn as_function(&self) -> Option<ir::FunctionID> {
        match self {
            DynValue::Function(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_struct_mut(&mut self, struct_id: ir::TypeDefID) -> Option<&mut StructValue> {
        match self {
            DynValue::Structure(struct_val) if struct_id == struct_val.type_id => Some(struct_val),
            _ => None,
        }
    }

    pub fn as_struct(&self, struct_id: ir::TypeDefID) -> Option<&StructValue> {
        match self {
            DynValue::Structure(struct_val) if struct_id == struct_val.type_id => Some(struct_val),
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
            DynValue::Array(arr) if arr.el_ty == *el_ty => Some(&arr.elements),
            _ => None,
        }
    }

    pub fn as_variant(&self, struct_id: ir::TypeDefID) -> Option<&VariantValue> {
        match self {
            DynValue::Variant(var_val) if struct_id == var_val.type_id => Some(var_val),
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
    pub type_id: ir::TypeDefID,
    pub rc: Option<RcState>,
    pub fields: Vec<DynValue>,
}

impl StructValue {
    pub fn new(id: ir::TypeDefID, fields: impl IntoIterator<Item=DynValue>) -> Self {
        Self {
            type_id: id,
            fields: fields.into_iter().collect(),

            // will be set by rc_alloc
            rc: None,
        }
    }
    
    pub fn struct_ty(&self) -> ir::Type {
        ir::Type::Struct(self.type_id)
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
        if self.type_id != other.type_id || self.fields.len() != other.fields.len() {
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

#[derive(Debug, Clone)]
pub struct RcState {
    pub strong_count: i32,
    pub weak_count: i32,
}

impl RcState {
    pub fn immortal() -> Self {
        Self {
            strong_count: -1,
            weak_count: 0,
        }
    }
    
    pub fn new(immortal: bool) -> Self {
        Self {
            strong_count: if immortal { -1 } else { 1 },
            weak_count: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariantValue {
    pub type_id: ir::TypeDefID,
    pub tag: Box<DynValue>,
    pub data: Box<DynValue>,
}

impl VariantValue {
    pub fn variant_ty(&self) -> ir::Type {
        ir::Type::Variant(self.type_id)
    }
}

#[derive(Debug, Clone)]
pub struct ArrayValue {
    pub el_ty: ir::Type,
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
        self.el_ty.clone().array(self.elements.len())
    }
}
