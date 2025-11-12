use crate::func::ffi::FfiInvoker;
use crate::ir;
use crate::ArrayValue;
use crate::DynValue;
use crate::ObjectHeader;
use crate::Pointer;
use crate::StructValue;
use crate::VariantValue;
use bimap::BiHashMap;
use ::dlopen::raw as dlopen;
use ::dlopen::Error as DlopenError;
use ir::IRFormatter;
use ir::RawInstructionFormatter;
use libffi::low::ffi_type;
use libffi::middle::Type as FfiType;
use libffi::middle::Builder as FfiBuilder;
use libffi::raw::FFI_TYPE_STRUCT;
use std::cmp::max;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::convert::TryInto;
use std::env;
use std::fmt;
use std::iter;
use std::mem::size_of;
use std::mem::transmute;
use std::ptr::slice_from_raw_parts;
use std::ptr::slice_from_raw_parts_mut;
use std::rc::Rc;
use thiserror::Error;

const RC_ELEMENT_COUNT: usize = 3;

#[derive(Clone, Debug, Error)]
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
    InvalidTypeIndex {
        type_index: u64,
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
        Fmt: IRFormatter,
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
            MarshalError::InvalidTypeIndex { type_index: id }  => {
                write!(f, "unknown type ID in array: {}", id)
            }
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
                field, struct_id
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

// dynamic array values in memory are marshalled as a set of header fields followed by a variably
// sized sequence of elements
#[derive(Clone, Debug)]
pub struct DynArrayHeader {
    pub rc: ObjectHeader,
    pub len: i32,
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

#[derive(Debug, Clone)]
pub struct StructFieldInfo {
    pub offset: usize,
    pub ty: ir::Type,
    pub foreign_ty: ForeignType,
}

#[derive(Debug, Clone)]
pub struct Marshaller {
    types: HashMap<ir::Type, ForeignType>,
    libs: HashMap<String, Rc<dlopen::Library>>,

    struct_field_maps: BTreeMap<ir::TypeDefID, BTreeMap<ir::FieldID, StructFieldInfo>>,
    variant_case_types: BTreeMap<ir::TypeDefID, Vec<Option<ir::Type>>>,

    // structure types that need refcounting fields and type info for virtual calls
    ref_types: BTreeSet<ir::TypeDefID>,
    
    // map of known types to serializable type indices
    next_type_index: u64,
    type_indices: BiHashMap<u64, ir::Type>,
}

impl Marshaller {
    pub fn new() -> Self {
        let mut marshaller = Self {
            types: HashMap::new(),
            libs: HashMap::new(),

            struct_field_maps: BTreeMap::new(),
            variant_case_types: BTreeMap::new(),
            
            next_type_index: 0,
            type_indices: BiHashMap::new(),

            ref_types: BTreeSet::new(),
        };

        let primitive_types = [
            (ir::Type::Nothing.ptr(), ForeignType::pointer()),
            (ir::Type::I8, ForeignType::i8()),
            (ir::Type::U8, ForeignType::u8()),
            (ir::Type::I16, ForeignType::i16()),
            (ir::Type::U16, ForeignType::u16()),
            (ir::Type::I32, ForeignType::i32()),
            (ir::Type::U32, ForeignType::u32()),
            (ir::Type::I64, ForeignType::i64()),
            (ir::Type::U64, ForeignType::u64()),
            (ir::Type::ISize, ForeignType::isize()),
            (ir::Type::USize, ForeignType::usize()),
            (ir::Type::F32, ForeignType::f32()),
            (ir::Type::F64, ForeignType::f64()),
            (ir::Type::Bool, ForeignType::u8()),
            (ir::Type::any(), ForeignType::pointer()),
        ];

        for (primitive_type, ffi_type) in primitive_types {
            marshaller.add_type_index(primitive_type.clone(), ffi_type.clone());
            marshaller.add_type_index(primitive_type.clone().ptr(), ForeignType::pointer());
            
            marshaller.add_dyn_array_type(primitive_type);
        }

        marshaller
    }

    pub fn variant_tag_type(&self) -> ForeignType {
        ForeignType::i32()
    }
    
    fn add_type_index(&mut self, ty: ir::Type, ffi_ty: ForeignType) {
        if self.type_indices.contains_right(&ty) {
            return;
        }

        self.types.insert(ty.clone(), ffi_ty);
        self.type_indices.insert(self.next_type_index, ty);
        
        self.next_type_index += 1;
    }
    
    fn add_dyn_array_type(&mut self, element_type: ir::Type) {
        self.add_type_index(element_type.clone().dyn_array(), ForeignType::pointer());

        // this will never be marshalled directly, but is used to tag dynamic array objects
        self.add_type_index(element_type.array(0), ForeignType(FfiType::void()));
    }

    pub fn add_struct(
        &mut self,
        id: ir::TypeDefID,
        def: &ir::StructDef,
        metadata: &ir::Metadata,
    ) -> MarshalResult<ForeignType> {
        let struct_ty = ir::Type::Struct(id);
        if let Some(cached) = self.types.get(&struct_ty) {
            return Ok(cached.clone());
        }

        let mut def_fields: Vec<_> = def.fields.iter().collect();
        def_fields.sort_by_key(|(id, _)| **id);
        
        // definitions may have gaps in their field lists, in which case we need to remember
        // the element index of each field so we can find it without expecting the field ID to match
        
        let mut element_count = def_fields.len();
        if def.identity.is_ref_type() {
            element_count += RC_ELEMENT_COUNT;
        }

        let mut field_ffi_tys = Vec::with_capacity(element_count);
        let mut def_field_tys = BTreeMap::new();

        if def.identity.is_ref_type() {
            field_ffi_tys.push(ForeignType::usize()); // type ID
            field_ffi_tys.push(ForeignType::i32()); // strong rc
            field_ffi_tys.push(ForeignType::i32()); // weak rc

            self.ref_types.insert(id);
        }
        
        let mut offset = field_ffi_tys.iter()
            .map(|ty| ty.size())
            .sum::<usize>();

        for (field_id, field_def) in def_fields {
            let foreign_ty = self.build_marshalled_type(&field_def.ty, metadata)?;
            field_ffi_tys.push(foreign_ty.clone());
            
            let size = foreign_ty.size();

            def_field_tys.insert(*field_id, StructFieldInfo {
                offset,
                ty: field_def.ty.clone(),
                foreign_ty,
            });

            offset += size;
        }

        self.struct_field_maps.insert(id, def_field_tys);

        match &def.identity {
            // value types: array contains this type directly as an element
            ir::StructIdentity::Record(..) 
            | ir::StructIdentity::SetFlags { .. } 
            | ir::StructIdentity::Array(..) => {
                self.add_dyn_array_type(struct_ty.clone());
            }
            
            // class types: array contains object pointers
            ir::StructIdentity::Class(..) => {
                self.add_dyn_array_type(id.to_class_ptr_type());
            }
            
            ir::StructIdentity::Closure(identity) => {
                self.add_dyn_array_type(identity.to_closure_ptr_type());
            }
        }

        let struct_ffi_ty = ForeignType::structure(field_ffi_tys);
        self.add_type_index(struct_ty, struct_ffi_ty.clone());

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

        self.add_dyn_array_type(variant_ty.clone());
        self.add_type_index(variant_ty, variant_ffi_ty.clone());

        self.variant_case_types.insert(id, case_tys);

        Ok(variant_ffi_ty)
    }

    pub fn add_iface(&mut self, id: ir::InterfaceID) {
        let iface_ty = id.to_interface_ptr_type();
        
        self.add_dyn_array_type(iface_ty)
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
        
        self.add_dyn_array_type(flags_type.clone());
        self.add_type_index(flags_type, repr_ffi_ty.clone());
        
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
                let lib_filename = format!("{}{}{}", env::consts::DLL_PREFIX, func_ref.src, env::consts::DLL_SUFFIX);
                let lib = dlopen::Library::open(lib_filename)
                    .map_err(sym_load_err)?;

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
            | ir::Type::TempRef(..) 
            | ir::Type::Function(..) => {
                let pointer_type = ForeignType::pointer();
                
                self.add_dyn_array_type(ty.clone());
                self.add_type_index(ty.clone(), pointer_type.clone());
                
                Ok(pointer_type)
            },

            ir::Type::Array { element, dim } => {
                let el_ty = self.build_marshalled_type(&element, metadata)?;
                let el_tys = vec![el_ty; *dim];
                
                let array_struct = ForeignType::structure(el_tys);

                self.add_dyn_array_type(ty.clone());
                self.add_type_index(ty.clone(), array_struct.clone());

                Ok(array_struct)
            },

            ir::Type::Flags(ty_id, set_id) => {
                Ok(self.add_flags_type(*ty_id, *set_id, metadata)?)
            }

            // all primitives/builtins should be in the cache already
            _ => {
                Err(MarshalError::UnsupportedType(ty.clone()))
            },
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
            | ir::Type::TempRef(..) 
            | ir::Type::RcPointer(..) 
            | ir::Type::RcWeakPointer(..) 
            | ir::Type::Function(..) => Ok(ForeignType::pointer()),

            ty => match self.types.get(ty) {
                Some(cached_ty) => Ok(cached_ty.clone()),
                None => Err(MarshalError::UnsupportedType(ty.clone())),
            },
        }
    }

    pub fn get_field_info(&self, type_id: ir::TypeDefID, field: ir::FieldID) -> MarshalResult<&StructFieldInfo> {
        let fields = self.struct_field_maps
            .get(&type_id)
            .ok_or_else(|| {
                MarshalError::UnsupportedType(ir::Type::Struct(type_id))
            })?;

        let field_info = fields
            .get(&field)
            .ok_or_else(|| {
                MarshalError::FieldOutOfRange { struct_id: type_id, field }
            })?;

        Ok(field_info)
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
            DynValue::F64(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes),
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
            DynValue::I64(..) | DynValue::U64(..) | DynValue::F64(..) => 8,

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
                let el_size = self.get_ty(&array.element_type)?.size();
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

    pub fn unmarshal_dyn_array_header_at(&self, pointer: &Pointer) -> MarshalResult<DynArrayHeader> {
        if pointer.addr == 0 {
            return Err(MarshalError::InvalidData);
        }

        let mem_slice = slice_from_raw_parts(
            pointer.addr as *const u8,
            Self::array_header_size(),
        );

        let result = self.unmarshal_dyn_array_header(unsafe {
            mem_slice.as_ref().unwrap()
        })?;

        Ok(result.value)
    }

    pub fn unmarshal_at(&self, pointer: &Pointer) -> MarshalResult<DynValue> {
        if pointer.addr == 0 {
            return Err(MarshalError::InvalidData);
        }

        let marshal_ty = self.get_ty(&pointer.ty)?;

        let result = self.unmarshal(unsafe {
            pointer.as_slice(marshal_ty.size())
        }, &pointer.ty)?;

        Ok(result.value)
    }

    pub fn marshal_at(&self, val: &DynValue, into_ptr: &Pointer) -> MarshalResult<usize> {
        if into_ptr.addr == 0 {
            return Err(MarshalError::InvalidData);
        }

        let marshal_ty = self.get_ty(&into_ptr.ty)?;
        let type_size = marshal_ty.size();

        let mem_slice = slice_from_raw_parts_mut(into_ptr.addr as *mut u8, type_size);
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
            ir::Type::F64 => unmarshal_from_ne_bytes(in_bytes, f64::from_ne_bytes)?.map(DynValue::F64),

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

                // if we have an expected type, assume the pointer is of that type
                let ptr_ty = match class_id {
                    ir::VirtualTypeID::Class(type_id) => ir::Type::Struct(*type_id),

                    ir::VirtualTypeID::Array(element_type) => ir::Type::Array { 
                        element: element_type.clone(),
                        dim: 0,
                    },

                    _ => ir::Type::Nothing,
                };

                raw_ptr_val.map(|raw_ptr| {
                    let ptr = raw_ptr.reinterpret(ptr_ty);
                    DynValue::Pointer(ptr)
                })
            },

            ir::Type::Pointer(deref_ty) | ir::Type::TempRef(deref_ty) => {
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
                self.unmarshal_static_array(element, *dim, in_bytes)?
                    .map(|array| DynValue::Array(Box::new(array)))
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

    pub fn marshal_object_header(&self, header: &ObjectHeader, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let type_index = self.type_indices.get_by_right(&header.object_type)
            .ok_or_else(|| {
                MarshalError::UnsupportedType(header.object_type.clone())
            })?;
        
        let mut offset = 0;
        offset += marshal_bytes(&type_index.to_ne_bytes(), &mut out_bytes[offset..]);
        offset += marshal_bytes(&header.strong_count.to_ne_bytes(), &mut out_bytes[offset..]);
        offset += marshal_bytes(&header.weak_count.to_ne_bytes(), &mut out_bytes[offset..]);

        Ok(offset)
    }

    pub fn unmarshal_object_header(&self, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<ObjectHeader>> {
        let mut offset = 0;

        let type_index = unmarshal_from_ne_bytes(&in_bytes[offset..], u64::from_ne_bytes)?;
        offset += type_index.byte_count;

        let strong_count = unmarshal_from_ne_bytes(&in_bytes[offset..], i32::from_ne_bytes)?;
        offset += strong_count.byte_count;

        let weak_count = unmarshal_from_ne_bytes(&in_bytes[offset..], i32::from_ne_bytes)?;
        offset += weak_count.byte_count;
        
        let object_type = self.type_indices.get_by_left(&type_index.value)
            .ok_or_else(|| {
                MarshalError::InvalidTypeIndex { type_index: type_index.value }
            })?;

        Ok(UnmarshalledValue {
            value: ObjectHeader {
                object_type: object_type.clone(),
                strong_count: strong_count.value,
                weak_count: weak_count.value,
            },
            byte_count: offset,
        })
    }

    pub fn object_header_size() -> usize {
        ForeignType::u64().size() // type index
            + ForeignType::i32().size() // strong ref count
            + ForeignType::i32().size() // weak ref count
    }
    
    // the array header is the region of memory preceding the elements of a dynamic array
    pub fn array_header_size() -> usize {
        let header_size = Self::object_header_size()
            + ForeignType::i32().size(); // element count
        
        header_size
    }

    pub(crate) fn marshal_array(&self, array_value: &ArrayValue, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let mut offset = 0;

        if let Some(rc) = &array_value.rc {
            offset += self.marshal_object_header(rc, &mut out_bytes[offset..])?;
            
            let len = i32::try_from(array_value.elements.len())
                .map_err(|_| {
                    // length can't be converted to i32
                    MarshalError::UnsupportedValue(DynValue::Array(Box::new(array_value.clone())))
                })?;
            
            offset += marshal_bytes(&len.to_ne_bytes(), &mut out_bytes[offset..]);
        }
        
        for element in &array_value.elements {
            offset += self.marshal(element, &mut out_bytes[offset..])?;
        }
        
        Ok(offset)
    }
    
    fn unmarshal_dyn_array_header(&self, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<DynArrayHeader>> {
        let mut offset = 0;

        // non-static arrays must be RC objects
        let rc = self.unmarshal_object_header(&in_bytes[offset..])?;
        offset += rc.byte_count;

        assert!(matches!(rc.value.object_type, ir::Type::Array { dim: 0, .. }));

        let size_int = unmarshal_from_ne_bytes(&in_bytes[offset..], i32::from_ne_bytes)?;
        offset += size_int.byte_count;
        
        Ok(UnmarshalledValue {
            value: DynArrayHeader {
                rc: rc.value,
                len: size_int.value,
            },
            byte_count: offset,
        })
    }
    
    fn unmarshal_static_array(&self, element_type: &ir::Type, size: usize, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<ArrayValue>> {
        let mut offset = 0;
        
        let mut elements = Vec::with_capacity(size);
        
        for _ in 0..size {
            let element = self.unmarshal(&in_bytes[offset..], element_type)?;
            
            elements.push(element.value);
            offset += element.byte_count;
        }

        Ok(UnmarshalledValue {
            byte_count: offset,
            value: ArrayValue {
                element_type: element_type.clone(),
                elements,
                rc: None,
            },
        })
    }

    fn marshal_struct(&self, struct_val: &StructValue, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let mut offset = 0;
        if let Some(rc_state) = struct_val.rc.as_ref() {
            offset += self.marshal_object_header(rc_state, &mut out_bytes[offset..])?;
        }
        
        let Some(fields) = self.struct_field_maps.get(&struct_val.type_id) else {
            // struct type is not in metadata
            return Err(MarshalError::UnsupportedType(struct_val.type_id.to_struct_type()));
        };

        for (field_id, field_info) in fields {
            match struct_val.fields.get(field_id.0) {
                Some(field_val) => {
                    offset += self.marshal(field_val, &mut out_bytes[offset..])?;
                }
                
                // skip this field
                None => {
                    let field_foreign_ty = self.get_ty(&field_info.ty)?;
                    offset += field_foreign_ty.size();
                }
            };
        }

        Ok(offset)
    }

    fn unmarshal_struct(&self, struct_id: ir::TypeDefID, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<StructValue>> {
        let mut offset = 0;

        let rc = if self.ref_types.contains(&struct_id) {
            let rc = self.unmarshal_object_header(in_bytes)?;
            offset += rc.byte_count;

            assert_eq!(struct_id.to_struct_type(), rc.value.object_type);

            Some(rc.value)
        } else {
            None
        };

        let field_map = self.struct_field_maps
            .get(&struct_id)
            .ok_or_else(|| {
                MarshalError::UnsupportedType(ir::Type::Struct(struct_id))
            })?;

        let fields_len = field_map.keys()
            .map(|id| id.0)
            .max()
            .map(|max_id| max_id + 1)
            .unwrap_or(0);

        let mut fields = vec![DynValue::I32(-1); fields_len];

        for (id, field_info) in field_map {
            let field_val = self.unmarshal(&in_bytes[offset..], &field_info.ty)?;
            offset += field_val.byte_count;
            fields[id.0] = field_val.value;
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
            .ok_or_else(|| {
                MarshalError::UnsupportedType(ir::Type::Variant(variant_val.type_id))
            })?;

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
            .ok_or_else(|| {
                MarshalError::UnsupportedType(ir::Type::Variant(variant_id))
            })?;

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
