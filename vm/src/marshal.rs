use crate::func::ffi::FfiInvoker;
use crate::ir;
use crate::ArrayValue;
use crate::DynValue;
use crate::Pointer;
use crate::RcState;
use crate::StructValue;
use crate::VariantValue;
use ::dlopen::raw as dlopen;
use ::dlopen::Error as DlopenError;
use ir::InstructionFormatter;
use ir::RawInstructionFormatter;
use libffi::low::ffi_type;
use libffi::middle::Builder as FfiBuilder;
use libffi::middle::Type as FfiType;
use libffi::raw::FFI_TYPE_STRUCT;
use std::cmp::max;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt;
use std::iter;
use std::mem::size_of;
use std::mem::transmute;
use std::ptr::slice_from_raw_parts;
use std::ptr::slice_from_raw_parts_mut;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum MarshalError {
    InvalidData,

    UnsupportedType(ir::Type),
    UnsupportedValue(DynValue),

    VariantTagOutOfRange {
        variant_id: ir::TypeDefID,
        tag: DynValue,
    },
    FieldOutOfRange {
        struct_id: ir::TypeDefID,
        field: ir::FieldID,
    },

    InvalidStructID {
        expected: ir::TypeDefID,
        actual: ir::TypeDefID,
    },

    InvalidRefCountValue(DynValue),

    ExternSymbolLoadFailed {
        lib: String,
        symbol: String,
        msg: String,
    },
}

impl fmt::Display for MarshalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_pretty(f, &RawInstructionFormatter)
    }
}

impl MarshalError {
    pub fn fmt_pretty<Fmt>(&self, f: &mut fmt::Formatter, format: &Fmt) -> fmt::Result
    where
        Fmt: InstructionFormatter,
    {
        match self {
            MarshalError::InvalidData => write!(f, "invalid data"),
            MarshalError::InvalidStructID { expected, actual } => {
                write!(f, "expected struct ")?;
                format.format_type(&ir::Type::Struct(*expected), f)?;
                write!(f, ", got ")?;
                format.format_type(&ir::Type::Struct(*actual), f)?;
                Ok(())
            },
            MarshalError::UnsupportedValue(val) => {
                write!(f, "unable to marshal value: {:?}", val)
            },
            MarshalError::UnsupportedType(ty) => {
                write!(f, "unable to marshal type: ")?;
                format.format_type(ty, f)?;
                Ok(())
            },
            MarshalError::ExternSymbolLoadFailed { lib, symbol, msg } => write!(
                f,
                "external symbol {}!{} failed to load ({})",
                lib, symbol, msg
            ),
            MarshalError::VariantTagOutOfRange { variant_id, tag } => write!(
                f,
                "tag {:?} for variant {} was out of range",
                tag, variant_id
            ),
            MarshalError::FieldOutOfRange { struct_id, field } => write!(
                f,
                "field {} for struct {} was out of range",
                struct_id, field
            ),
            MarshalError::InvalidRefCountValue(val) => {
                write!(f, "value is not a valid ref count: {:?}", val)
            },
        }
    }
}

pub type MarshalResult<T> = Result<T, MarshalError>;

#[derive(Debug, Clone)]
pub struct UnmarshalledValue<T> {
    pub value: T,
    pub byte_count: usize,
}

impl From<UnmarshalledValue<DynValue>> for DynValue {
    fn from(val: UnmarshalledValue<DynValue>) -> Self {
        val.value
    }
}

impl<T> UnmarshalledValue<T> {
    pub fn map<U, F>(self, f: F) -> UnmarshalledValue<U>
    where
        F: FnOnce(T) -> U,
    {
        UnmarshalledValue {
            value: f(self.value),
            byte_count: self.byte_count,
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Debug)]
pub struct ForeignType(FfiType);

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

    pub fn size(&self) -> usize {
        let raw_ty = self.0.as_raw_ptr();

        unsafe {
            if (*raw_ty).type_ as u32 == FFI_TYPE_STRUCT {
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

#[derive(Debug, Clone)]
pub struct ForeignFieldInfo {
    pub offset: usize,
    pub ty: ir::Type,
    pub foreign_ty: ForeignType,
}

#[derive(Debug, Clone)]
pub struct Marshaller {
    types: HashMap<ir::Type, ForeignType>,
    libs: HashMap<String, Rc<dlopen::Library>>,

    struct_field_types: BTreeMap<ir::TypeDefID, Vec<ir::Type>>,
    variant_case_types: BTreeMap<ir::TypeDefID, Vec<Option<ir::Type>>>,

    // structure types that need refcounting fields and type info for virtual calls
    ref_types: BTreeSet<ir::TypeDefID>,
}

impl Marshaller {
    pub fn new() -> Self {
        let mut marshaller = Self {
            types: HashMap::new(),
            libs: HashMap::new(),

            struct_field_types: BTreeMap::new(),
            variant_case_types: BTreeMap::new(),

            ref_types: BTreeSet::new(),
        };

        marshaller.types.insert(ir::Type::I8, ForeignType::i8());
        marshaller.types.insert(ir::Type::U8, ForeignType::u8());
        marshaller.types.insert(ir::Type::I16, ForeignType::i16());
        marshaller.types.insert(ir::Type::U16, ForeignType::u16());
        marshaller.types.insert(ir::Type::I32, ForeignType::i32());
        marshaller.types.insert(ir::Type::U32, ForeignType::u32());
        marshaller.types.insert(ir::Type::I64, ForeignType::i64());
        marshaller.types.insert(ir::Type::U64, ForeignType::u64());
        marshaller.types.insert(ir::Type::ISize, ForeignType::isize());
        marshaller.types.insert(ir::Type::USize, ForeignType::usize());
        marshaller.types.insert(ir::Type::F32, ForeignType::f32());
        marshaller.types.insert(ir::Type::Bool, ForeignType::u8());

        marshaller
    }

    pub fn variant_tag_type(&self) -> ForeignType {
        ForeignType::i32()
    }

    pub fn add_struct(
        &mut self,
        id: ir::TypeDefID,
        def: &ir::Struct,
        metadata: &ir::Metadata,
    ) -> MarshalResult<ForeignType> {
        let struct_ty = ir::Type::Struct(id);
        if let Some(cached) = self.types.get(&struct_ty) {
            return Ok(cached.clone());
        }

        let mut def_fields: Vec<_> = def.fields.iter().collect();
        def_fields.sort_by_key(|(f_id, _)| **f_id);
        let def_field_tys: Vec<_> = def_fields
            .into_iter()
            .map(|(_, field)| field.ty.clone())
            .collect();

        let mut field_ffi_tys = Vec::with_capacity(def_field_tys.len());

        if def.identity.is_ref_type() {
            field_ffi_tys.push(ForeignType::usize()); // type ID
            field_ffi_tys.push(ForeignType::i32()); // strong rc
            field_ffi_tys.push(ForeignType::i32()); // weak rc

            self.ref_types.insert(id);
        }

        for def_field_ty in &def_field_tys {
            field_ffi_tys.push(self.build_marshalled_type(def_field_ty, metadata)?);
        }

        let struct_ffi_ty = ForeignType::structure(field_ffi_tys.iter().cloned());

        self.types.insert(struct_ty, struct_ffi_ty.clone());
        self.struct_field_types.insert(id, def_field_tys);

        Ok(struct_ffi_ty)
    }

    pub fn add_variant(
        &mut self,
        id: ir::TypeDefID,
        def: &ir::VariantDef,
        metadata: &ir::Metadata,
    ) -> MarshalResult<ForeignType> {
        let variant_ty = ir::Type::Variant(id);
        if let Some(cached) = self.types.get(&variant_ty) {
            return Ok(cached.clone());
        }

        let case_tys: Vec<_> = def.cases.iter().map(|case| case.ty.clone()).collect();

        let mut case_ffi_tys = Vec::with_capacity(case_tys.len());
        let mut max_case_size = 0;

        for case_ty in &case_tys {
            if let Some(case_ty) = case_ty.as_ref() {
                let case_ffi_ty = self.build_marshalled_type(case_ty, metadata)?;

                max_case_size = max(case_ffi_ty.size(), max_case_size);

                case_ffi_tys.push(case_ffi_ty);
            };
        }

        let variant_layout: Vec<_> = iter::once(self.variant_tag_type())
            .chain(iter::repeat(ForeignType::u8()).take(max_case_size))
            .collect();

        let variant_ffi_ty = ForeignType::structure(variant_layout);

        self.types.insert(variant_ty, variant_ffi_ty.clone());

        self.variant_case_types.insert(id, case_tys);

        Ok(variant_ffi_ty)
    }
    
    pub fn add_flags_type(
        &mut self,
        repr_ty_id: ir::TypeDefID,
        set_id: ir::SetAliasID,
        metadata: &ir::Metadata
    ) -> MarshalResult<ForeignType> {
        let repr_struct_ty = ir::Type::Struct(repr_ty_id);
        let repr_ffi_ty = self.build_marshalled_type(&repr_struct_ty, metadata)?;

        let flags_type = ir::Type::Flags(repr_ty_id, set_id);
        self.types.insert(flags_type, repr_ffi_ty.clone());
        
        Ok(repr_ffi_ty)
    }

    pub fn build_ffi_invoker(
        &mut self,
        func_ref: &ir::ExternalFunctionRef,
        metadata: &ir::Metadata,
    ) -> MarshalResult<FfiInvoker> {
        let sym_load_err = |err: DlopenError| MarshalError::ExternSymbolLoadFailed {
            lib: func_ref.src.clone(),
            symbol: func_ref.symbol.clone(),
            msg: err.to_string(),
        };

        // the "nothing" type is usually not allowed by the marshaller because it can't be
        // instantiated, but here we need to map it to the void ffi type
        let ffi_return_ty = match &func_ref.sig.return_ty {
            ir::Type::Nothing => ForeignType(FfiType::void()),
            return_ty => self.build_marshalled_type(return_ty, metadata)?,
        };

        let ffi_param_tys: Vec<_> = func_ref
            .sig
            .param_tys
            .iter()
            .map(|ty| self.build_marshalled_type(ty, metadata))
            .collect::<MarshalResult<_>>()?;

        let cif = FfiBuilder::new()
            .args(ffi_param_tys.iter().map(|t| t.0.clone()))
            .res(ffi_return_ty.0.clone())
            .into_cif();

        let lib = match self.libs.get(&func_ref.src) {
            Some(lib_rc) => lib_rc.clone(),
            None => {
                let lib = dlopen::Library::open(&func_ref.src).map_err(sym_load_err)?;

                let lib_rc = Rc::new(lib);
                self.libs.insert(func_ref.src.clone(), lib_rc.clone());
                lib_rc
            },
        };

        let symbol = unsafe {
            lib.symbol::<*const ()>(&func_ref.symbol)
                .map_err(sym_load_err)?
        };

        Ok(FfiInvoker::new(
            cif,
            symbol,
            ffi_param_tys,
            func_ref.sig.return_ty.clone(),
            ffi_return_ty,
        ))
    }

    fn build_marshalled_type(
        &mut self,
        ty: &ir::Type,
        metadata: &ir::Metadata,
    ) -> MarshalResult<ForeignType> {
        if let Some(cached) = self.types.get(ty) {
            return Ok(cached.clone());
        }

        match ty {
            ir::Type::Variant(id) => {
                let def = metadata
                    .get_variant_def(*id)
                    .ok_or_else(|| {
                        MarshalError::UnsupportedType(ty.clone())
                    })?;

                self.add_variant(*id, def, metadata)
            },

            ir::Type::Struct(id) => {
                let def = metadata
                    .get_struct_def(*id)
                    .ok_or_else(|| {
                        MarshalError::UnsupportedType(ty.clone())
                    })?;

                self.add_struct(*id, def, metadata)
            },

            ir::Type::RcPointer(..) 
            | ir::Type::RcWeakPointer(..) 
            | ir::Type::Pointer(..) 
            | ir::Type::Function(..) => Ok(ForeignType::pointer()),

            ir::Type::Array { element, dim } => {
                let el_ty = self.build_marshalled_type(&element, metadata)?;
                let el_tys = vec![el_ty; *dim];

                Ok(ForeignType::structure(el_tys))
            },

            ir::Type::Flags(ty_id, set_id) => {
                self.add_flags_type(*ty_id, *set_id, metadata)
            }

            // all primitives/builtins should be in the cache already
            _ => Err(MarshalError::UnsupportedType(ty.clone())),
        }
    }

    pub fn get_ty(&self, ty: &ir::Type) -> MarshalResult<ForeignType> {
        match ty {
            ir::Type::Nothing => {
                // "nothing" is not a marshallable type!
                Err(MarshalError::UnsupportedType(ir::Type::Nothing))
            },

            // we can't cache array types so always build them on demand
            // pascal static arrays are treated as a struct of elements laid out sequentially
            ir::Type::Array { element, dim } => {
                let el_ty = self.get_ty(&element)?;
                let el_tys = vec![el_ty; *dim];

                Ok(ForeignType::structure(el_tys))
            },

            ir::Type::Pointer(..) 
            | ir::Type::RcPointer(..) 
            | ir::Type::RcWeakPointer(..) 
            | ir::Type::Function(..) => Ok(ForeignType::pointer()),

            ty => match self.types.get(ty) {
                Some(cached_ty) => Ok(cached_ty.clone()),
                None => Err(MarshalError::UnsupportedType(ty.clone())),
            },
        }
    }

    pub fn get_field_info(&self, type_id: ir::TypeDefID, field: ir::FieldID) -> MarshalResult<ForeignFieldInfo> {
        let struct_marshal_ty = self.get_ty(&ir::Type::Struct(type_id))?;

        let fields = self.struct_field_types.get(&type_id)
            .ok_or_else(|| MarshalError::UnsupportedType(ir::Type::Struct(type_id)))?;
        let field_ty = fields.get(field.0)
            .ok_or_else(|| MarshalError::FieldOutOfRange { struct_id: type_id, field })?
            .clone();

        let field_index = if self.ref_types.contains(&type_id) {
            field.0 + 3
        } else {
            field.0
        };

        let struct_elements = struct_marshal_ty.elements();
        let field_offset = struct_elements
            .iter()
            .take(field_index)
            .map(|field_ty| field_ty.size())
            .sum();
        let ffi_ty = struct_elements[field_index].clone();

        Ok(ForeignFieldInfo {
            offset: field_offset,
            ty: field_ty,
            foreign_ty: ffi_ty,
        })
    }

    pub fn marshal(&self, val: &DynValue, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let size = match val {
            DynValue::I8(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::U8(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::I16(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::U16(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::I32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::U32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::I64(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::U64(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::ISize(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::USize(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::F32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
            DynValue::Bool(x) => {
                out_bytes[0] = if *x { 1 } else { 0 };
                1
            },

            DynValue::Array(x) => {
                let mut el_offset = 0;

                for i in 0..x.elements.len() {
                    let el = &x.elements[i];
                    let el_bytes = &mut out_bytes[el_offset..];
                    el_offset += self.marshal(el, el_bytes)?;
                }

                el_offset
            },

            DynValue::Structure(struct_val) => {
                self.marshal_struct(struct_val, out_bytes)?
            },

            DynValue::Variant(variant_val) => self.marshal_variant(variant_val, out_bytes)?,

            DynValue::Pointer(ptr) => self.marshal_ptr(ptr, out_bytes)?,

            DynValue::Function(func_id) => {
                marshal_bytes(&func_id.0.to_ne_bytes(), out_bytes)
            }
        };

        Ok(size)
    }
    
    pub fn marshal_to_vec(&self, val: &DynValue) -> MarshalResult<Vec<u8>> {
        // have to calculate the size before writing
        let capacity = match &val {
            DynValue::Bool(..) => 1,
            DynValue::I8(..) | DynValue::U8(..) => 1,
            DynValue::I16(..) | DynValue::U16(..) => 2,
            DynValue::I32(..) | DynValue::U32(..) | DynValue::F32(..) => 4,
            DynValue::I64(..) | DynValue::U64(..) => 8,

            DynValue::Pointer(..) 
            | DynValue::Function(..)
            | DynValue::ISize(..) 
            | DynValue::USize(..) => size_of::<usize>(),
    
            DynValue::Structure(structure) => {
                let ty = self.get_ty(&ir::Type::Struct(structure.type_id))?;
                ty.size()
            }

            DynValue::Variant(variant) => {
                let ty = self.get_ty(&ir::Type::Struct(variant.type_id))?;
                ty.size()
            }

            DynValue::Array(array) => {
                let el_size = self.get_ty(&array.el_ty)?.size();
                array.elements.len() * el_size
            }
        };
        
        let mut bytes = vec![0u8; capacity];
        self.marshal(val, &mut bytes)?;
        
        Ok(bytes)
    } 

    fn marshal_ptr(&self, ptr: &Pointer, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let ptr_bytes = ptr.addr.to_ne_bytes();
        let len = marshal_bytes(&ptr_bytes, out_bytes);
        Ok(len)
    }

    fn unmarshal_ptr(&self, deref_ty: ir::Type, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<Pointer>> {
        let addr_bytes = unmarshal_bytes(in_bytes)?;
        let addr = usize::from_ne_bytes(addr_bytes);
        let ptr = Pointer {
            addr,
            ty: deref_ty.clone(),
        };

        Ok(UnmarshalledValue {
            value: ptr,
            byte_count: addr_bytes.len(),
        })
    }

    pub fn unmarshal_from_ptr(&self, into_ptr: &Pointer) -> MarshalResult<DynValue> {
        if into_ptr.addr == 0 {
            return Err(MarshalError::InvalidData);
        }

        let marshal_ty = self.get_ty(&into_ptr.ty)?;

        let mem_slice = slice_from_raw_parts(into_ptr.addr as *const u8, marshal_ty.size());
        let result = unsafe { self.unmarshal(mem_slice.as_ref().unwrap(), &into_ptr.ty)? };

        Ok(result.value)
    }

    pub fn marshal_into(&self, val: &DynValue, into_ptr: &Pointer) -> MarshalResult<usize> {
        if into_ptr.addr == 0 {
            return Err(MarshalError::InvalidData);
        }

        let marshal_ty = self.get_ty(&into_ptr.ty)?;

        let mem_slice = slice_from_raw_parts_mut(into_ptr.addr as *mut u8, marshal_ty.size());
        let size = unsafe { self.marshal(val, mem_slice.as_mut().unwrap())? };

        Ok(size)
    }

    pub fn unmarshal(
        &self,
        in_bytes: &[u8],
        ty: &ir::Type,
    ) -> MarshalResult<UnmarshalledValue<DynValue>> {
        let dyn_val = match ty {
            ir::Type::I8 => unmarshal_from_ne_bytes(in_bytes, i8::from_ne_bytes)?.map(DynValue::I8),
            ir::Type::U8 => unmarshal_from_ne_bytes(in_bytes, u8::from_ne_bytes)?.map(DynValue::U8),
            ir::Type::I16 => unmarshal_from_ne_bytes(in_bytes, i16::from_ne_bytes)?.map(DynValue::I16),
            ir::Type::U16 => unmarshal_from_ne_bytes(in_bytes, u16::from_ne_bytes)?.map(DynValue::U16),
            ir::Type::I32 => unmarshal_from_ne_bytes(in_bytes, i32::from_ne_bytes)?.map(DynValue::I32),
            ir::Type::U32 => unmarshal_from_ne_bytes(in_bytes, u32::from_ne_bytes)?.map(DynValue::U32),
            ir::Type::I64 => unmarshal_from_ne_bytes(in_bytes, i64::from_ne_bytes)?.map(DynValue::I64),
            ir::Type::U64 => unmarshal_from_ne_bytes(in_bytes, u64::from_ne_bytes)?.map(DynValue::U64),
            ir::Type::ISize => {
                unmarshal_from_ne_bytes(in_bytes, isize::from_ne_bytes)?.map(DynValue::ISize)
            },
            ir::Type::USize => {
                unmarshal_from_ne_bytes(in_bytes, usize::from_ne_bytes)?.map(DynValue::USize)
            },

            ir::Type::F32 => unmarshal_from_ne_bytes(in_bytes, f32::from_ne_bytes)?.map(DynValue::F32),

            ir::Type::Bool => {
                if in_bytes.len() == 0 {
                    return Err(MarshalError::InvalidData);
                }

                let value = DynValue::Bool(in_bytes[0] != 0);
                UnmarshalledValue {
                    value,
                    byte_count: 1,
                }
            },

            ir::Type::RcPointer(class_id)
            | ir::Type::RcWeakPointer(class_id) => {
                let raw_ptr_val = self.unmarshal_ptr(ir::Type::Nothing, in_bytes)?;

                // null rcpointers can exist - e.g. uninitialized stack values
                let ptr_ty = if raw_ptr_val.value.is_null() {
                    // if we have an expected concrete type, we can reinterpret the null pointer
                    // as that type as a minor optimization for later
                    if let ir::VirtualTypeID::Class(type_id) = class_id {
                        ir::Type::Struct(*type_id)
                    } else {
                        ir::Type::Nothing
                    }
                } else {
                    // the struct ID is the first field so we can just access it directly here
                    let struct_id_bytes = unsafe {
                        slice_from_raw_parts(raw_ptr_val.value.addr as *const u8, size_of::<usize>())
                            .as_ref()
                            .unwrap()
                    };
                    let struct_id = usize::from_ne_bytes(unmarshal_bytes(&struct_id_bytes)?);

                    ir::Type::Struct(ir::TypeDefID(struct_id))
                };

                raw_ptr_val.map(|raw_ptr| {
                    let ptr = raw_ptr.reinterpret(ptr_ty);
                    DynValue::Pointer(ptr)
                })
            },

            ir::Type::Pointer(deref_ty) => {
                self.unmarshal_ptr((**deref_ty).clone(), in_bytes)?
                    .map(DynValue::Pointer)
            },

            ir::Type::Function(..) => {
                let func_id = unmarshal_from_ne_bytes(in_bytes, usize::from_ne_bytes)?;

                func_id.map(|id| {
                    DynValue::Function(ir::FunctionID(id))
                })
            }

            ir::Type::Array { element, dim } => {
                let mut offset = 0;
                let mut elements = Vec::with_capacity(*dim);
                for _ in 0..*dim {
                    let el_val = self.unmarshal(&in_bytes[offset..], &element)?;
                    offset += el_val.byte_count;
                    elements.push(el_val.value);
                }

                UnmarshalledValue {
                    byte_count: offset,
                    value: DynValue::Array(Box::new(ArrayValue {
                        el_ty: (**element).clone(),
                        elements,
                    })),
                }
            },

            // these need field offset/tag type info from the ffi cache so marshal/unmarshal should
            // be members
            ir::Type::Struct(struct_id) | ir::Type::Flags(struct_id, ..) => {
                let struct_val = self.unmarshal_struct(*struct_id, in_bytes)?;

                UnmarshalledValue {
                    value: DynValue::Structure(Box::new(struct_val.value)),
                    byte_count: struct_val.byte_count,
                }
            },

            ir::Type::Variant(variant_id) => {
                let variant_val = self.unmarshal_variant(*variant_id, in_bytes)?;
                variant_val.map(Box::new).map(DynValue::Variant)
            },

            _ => {
                return Err(MarshalError::UnsupportedType(ty.clone()));
            },
        };

        Ok(dyn_val)
    }

    pub fn stack_alloc_size(&self, instructions: &[ir::Instruction]) -> MarshalResult<usize> {
        let mut local_sizes = Vec::new();
        for instruction in instructions {
            if let ir::Instruction::LocalAlloc(id, ty) = instruction {
                while local_sizes.len() <= id.0 {
                    local_sizes.push(0);
                }

                let ty_size = self.get_ty(ty)?.size();
                let local_size = &mut local_sizes[id.0];
                *local_size = max(ty_size, *local_size);
            }
        }

        let size_sum = local_sizes.into_iter().sum();
        Ok(size_sum)
    }

    fn marshal_struct(&self, struct_val: &StructValue, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let mut offset = 0;
        if let Some(rc_state) = struct_val.rc.as_ref() {
            assert!(self.ref_types.contains(&struct_val.type_id));

            offset += marshal_bytes(&struct_val.type_id.0.to_ne_bytes(), &mut out_bytes[offset..]);

            offset += marshal_bytes(&rc_state.strong_count.to_ne_bytes(), &mut out_bytes[offset..]);
            offset += marshal_bytes(&rc_state.weak_count.to_ne_bytes(), &mut out_bytes[offset..]);
        }

        for field in &struct_val.fields {
            offset += self.marshal(field, &mut out_bytes[offset..])?;
        }

        Ok(offset)
    }

    fn unmarshal_struct(&self, struct_id: ir::TypeDefID, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<StructValue>> {
        let mut offset = 0;

        let rc = if self.ref_types.contains(&struct_id) {
            // rc types start with a type ID we can skip
            offset += size_of::<usize>();
            let strong_count_val = unmarshal_from_ne_bytes(&in_bytes[offset..], i32::from_ne_bytes)?;
            offset += strong_count_val.byte_count;

            let weak_count_val = unmarshal_from_ne_bytes(&in_bytes[offset..], i32::from_ne_bytes)?;
            offset += weak_count_val.byte_count;

            Some(RcState {
                strong_count: strong_count_val.value,
                weak_count: weak_count_val.value,
            })
        } else {
            None
        };

        let field_tys = self
            .struct_field_types
            .get(&struct_id)
            .ok_or_else(|| MarshalError::UnsupportedType(ir::Type::Struct(struct_id)))?;

        let mut fields = Vec::new();

        for field_ty in field_tys {
            let field_val = self.unmarshal(&in_bytes[offset..], field_ty)?;
            offset += field_val.byte_count;
            fields.push(field_val.value);
        }

        Ok(UnmarshalledValue {
            byte_count: offset,
            value: StructValue {
                fields,
                type_id: struct_id,
                rc,
            },
        })
    }

    fn marshal_variant(
        &self,
        variant_val: &VariantValue,
        out_bytes: &mut [u8],
    ) -> MarshalResult<usize> {
        let case_tys = self
            .variant_case_types
            .get(&variant_val.type_id)
            .ok_or_else(|| MarshalError::UnsupportedType(ir::Type::Variant(variant_val.type_id)))?;

        let tag = variant_val
            .tag
            .as_i32()
            .ok_or_else(|| MarshalError::InvalidData)?;

        let case_ty = case_tys
            .get(tag as usize)
            .ok_or_else(|| MarshalError::InvalidData)?;

        // we still need to refer to the type size, because we always always marshal/unmarshal
        // the entire size of the variant regardless of which case is active
        let variant_size = self.get_ty(&ir::Type::Variant(variant_val.type_id))?.size();

        let tag_size = self.marshal(&variant_val.tag, out_bytes)?;

        // don't marshal anything for data-less cases
        let data_size = match case_ty {
            None => 0,
            Some(case_ty) => {
                let data_size = self.marshal(&variant_val.data, &mut out_bytes[tag_size..])?;
                let ty_size = self.get_ty(case_ty)?.size();
                assert_eq!(ty_size, data_size);

                data_size
            },
        };

        let marshalled_size = tag_size + data_size;
        assert!(
            marshalled_size <= variant_size,
            "marshalled size {} <= type size {} for variant {}",
            marshalled_size,
            variant_size,
            ir::Type::Variant(variant_val.type_id)
        );

        Ok(variant_size)
    }

    fn unmarshal_variant(
        &self,
        variant_id: ir::TypeDefID,
        in_bytes: &[u8],
    ) -> MarshalResult<UnmarshalledValue<VariantValue>> {
        let tag_val = self.unmarshal(in_bytes, &ir::Type::I32)?;
        let tag = tag_val.value.as_i32().unwrap();

        let case_index = cast::usize(tag).map_err(|_| MarshalError::VariantTagOutOfRange {
            variant_id,
            tag: tag_val.value.clone(),
        })?;

        let variant_size = self.get_ty(&ir::Type::Variant(variant_id))?.size();

        let case_tys = self
            .variant_case_types
            .get(&variant_id)
            .ok_or_else(|| MarshalError::UnsupportedType(ir::Type::Variant(variant_id)))?;

        let case_ty = case_tys
            .get(case_index)
            .ok_or_else(|| MarshalError::VariantTagOutOfRange {
                variant_id,
                tag: tag_val.value.clone(),
            })?
            .as_ref();

        let data_val = match case_ty {
            Some(case_ty) => self.unmarshal(&in_bytes[tag_val.byte_count..], case_ty)?,
            None => UnmarshalledValue {
                value: DynValue::Pointer(Pointer::nil(ir::Type::Nothing)),
                byte_count: 0,
            },
        };

        assert!(tag_val.byte_count + data_val.byte_count <= variant_size);

        Ok(UnmarshalledValue {
            byte_count: variant_size,
            value: VariantValue {
                type_id: variant_id,
                tag: Box::new(DynValue::I32(tag as i32)),
                data: Box::new(data_val.value),
            },
        })
    }
}

fn marshal_bytes(bytes: &[u8], out_bytes: &mut [u8]) -> usize {
    out_bytes[0..bytes.len()].copy_from_slice(bytes);
    bytes.len()
}

fn unmarshal_bytes<const COUNT: usize>(in_bytes: &[u8]) -> MarshalResult<[u8; COUNT]> {
    if in_bytes.len() < COUNT {
        return Err(MarshalError::InvalidData);
    }

    match in_bytes[0..COUNT].try_into() {
        Ok(bytes) => Ok(bytes),
        Err(..) => Err(MarshalError::InvalidData),
    }
}

fn unmarshal_from_ne_bytes<T, FUn, const COUNT: usize>(
    in_bytes: &[u8],
    f_un: FUn,
) -> MarshalResult<UnmarshalledValue<T>>
where
    FUn: Fn([u8; COUNT]) -> T,
{
    let bytes = unmarshal_bytes(in_bytes)?;
    let value = f_un(bytes);

    Ok(UnmarshalledValue {
        value,
        byte_count: bytes.len(),
    })
}
