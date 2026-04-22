use libffi::low::ffi_type;
use libffi::middle::Type as FfiType;
use libffi::raw::FFI_TYPE_STRUCT;
use std::fmt;
use std::mem::transmute;
use std::ptr::slice_from_raw_parts;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct TypeIndex(pub u64);

impl fmt::Display for TypeIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct ForeignType(pub(super) FfiType);

impl ForeignType {
    pub fn structure<I>(fields: I) -> Self
    where
        I: IntoIterator<Item = ForeignType>,
        I::IntoIter: ExactSizeIterator<Item = ForeignType>,
    {
        Self(FfiType::structure(fields.into_iter().map(|t| t.0)))
    }

    pub fn pointer() -> Self {
        Self(FfiType::pointer())
    }

    pub fn i8() -> Self {
        Self(FfiType::i8())
    }

    pub fn u8() -> Self {
        Self(FfiType::u8())
    }

    pub fn i16() -> Self {
        Self(FfiType::i16())
    }

    pub fn u16() -> Self {
        Self(FfiType::u16())
    }

    pub fn i32() -> Self {
        Self(FfiType::i32())
    }

    pub fn u32() -> Self {
        Self(FfiType::u32())
    }

    pub fn i64() -> Self {
        Self(FfiType::i64())
    }

    pub fn u64() -> Self {
        Self(FfiType::u64())
    }

    pub fn isize() -> Self {
        Self(FfiType::isize())
    }

    pub fn usize() -> Self {
        Self(FfiType::usize())
    }

    pub fn f32() -> Self {
        Self(FfiType::f32())
    }

    pub fn f64() -> Self {
        Self(FfiType::f64())
    }

    pub fn size(&self) -> usize {
        let raw_ty = self.0.as_raw_ptr();

        unsafe {
            if (*raw_ty).type_ == FFI_TYPE_STRUCT {
                let elements = self.elements();
                let mut total = 0;
                for element in elements {
                    total += element.size();
                }

                total
            } else {
                (*raw_ty).size
            }
        }
    }

    pub fn elements(&self) -> &[ForeignType] {
        let raw_ty = self.0.as_raw_ptr();

        unsafe {
            let mut i: usize = 0;
            let count = loop {
                let next_element: *mut *mut ffi_type = (*raw_ty).elements.offset(i as isize);
                if (*next_element).is_null() {
                    break i;
                } else {
                    i += 1;
                }
            };

            let slice = slice_from_raw_parts::<ForeignType>(transmute((*raw_ty).elements), count);
            slice.as_ref().unwrap()
        }
    }
}
