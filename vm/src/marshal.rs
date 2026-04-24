mod util;
mod error;
mod native_type;

use self::util::unmarshal_from_ne_bytes;
use self::util::UnmarshalledValue;
use crate::func::ffi::FfiInvoker;
use crate::ir;
use crate::ArrayValue;
use crate::DynValue;
use crate::ObjectHeader;
use crate::ObjectID;
use crate::ObjectValue;
use crate::Pointer;
use crate::StructValue;
use crate::VariantValue;
use bimap::BiHashMap;
use ::dlopen::raw as dlopen;
use ::dlopen::Error as DlopenError;
pub use error::*;
use ir::MetadataSource as _;
use libffi::middle::Builder as FfiBuilder;
use libffi::middle::Type as FfiType;
pub use native_type::*;
use std::borrow::Cow;
use std::cmp::max;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::convert::TryInto;
use std::env;
use std::error::Error;
use std::iter;
use std::mem::size_of;
use std::path::PathBuf;
use std::ptr::slice_from_raw_parts;
use std::ptr::slice_from_raw_parts_mut;
use std::rc::Rc;
use terapascal_ir::generic::instantiate_struct_def;

const RC_ELEMENT_COUNT: usize = 3;

// dynamic array values in memory are marshalled as a set of header fields followed by a variably
// sized sequence of elements
#[derive(Clone, Debug)]
pub struct DynArrayHeader {
    pub object_header: ObjectHeader,
    pub len: i32,
}

#[derive(Debug, Clone)]
pub struct StructFieldInfo {
    pub offset: usize,
    pub ty: ir::Type,
    pub foreign_ty: NativeType,
}

#[derive(Debug, Clone)]
struct StructLayout {
    def: Rc<ir::StructDef>,
    fields: BTreeMap<ir::FieldID, StructFieldInfo>,
}

#[derive(Debug, Clone)]
pub struct Marshaller {
    metadata: ir::Metadata,

    types: HashMap<TypeIndex, NativeType>,
    libs: HashMap<String, Rc<dlopen::Library>>,

    struct_layouts: BTreeMap<TypeIndex, StructLayout>,
    variant_case_types: BTreeMap<TypeIndex, Vec<Option<ir::Type>>>,

    // map of known object IDs to serializable indices
    next_type_index: TypeIndex,
    type_indices: BiHashMap<TypeIndex, ir::Type>,
    object_id_indices: BiHashMap<TypeIndex, ObjectID>,

    trace_generics: bool,
}

impl Marshaller {
    pub fn new(metadata: ir::Metadata, trace_generics: bool) -> MarshalResult<Self> {
        let mut marshaller = Self {
            metadata,

            types: HashMap::new(),
            libs: HashMap::new(),

            struct_layouts: BTreeMap::new(),
            variant_case_types: BTreeMap::new(),
            
            next_type_index: TypeIndex(0),
            object_id_indices: BiHashMap::new(),
            type_indices: BiHashMap::new(),

            trace_generics,
        };

        let primitive_types = [
            (ir::Type::Nothing.ptr(), NativeType::pointer()),
            (ir::Type::I8, NativeType::i8()),
            (ir::Type::U8, NativeType::u8()),
            (ir::Type::I16, NativeType::i16()),
            (ir::Type::U16, NativeType::u16()),
            (ir::Type::I32, NativeType::i32()),
            (ir::Type::U32, NativeType::u32()),
            (ir::Type::I64, NativeType::i64()),
            (ir::Type::U64, NativeType::u64()),
            (ir::Type::ISize, NativeType::isize()),
            (ir::Type::USize, NativeType::usize()),
            (ir::Type::F32, NativeType::f32()),
            (ir::Type::F64, NativeType::f64()),
            (ir::Type::Bool, NativeType::u8()),
            (ir::Type::any(), NativeType::pointer()),
            (ir::STRING_TYPE, NativeType::pointer()),
        ];

        for (primitive_type, ffi_type) in primitive_types {
            marshaller.register_type(primitive_type.clone(), ffi_type.clone())?;
            marshaller.register_type(primitive_type.clone().ptr(), NativeType::pointer())?;
            
            marshaller.add_dyn_array_type(primitive_type)?;
        }

        Ok(marshaller)
    }

    pub fn metadata(&self) -> &ir::Metadata {
        &self.metadata
    }

    pub fn load_metadata(&mut self, metadata: &ir::Metadata) -> MarshalResult<()> {
        self.metadata.merge_from(metadata);

        for (id, type_def) in metadata.type_defs() {
            match type_def {
                ir::TypeDef::Struct(struct_def) => {
                    if !struct_def.is_generic() {
                        self.add_struct_type(&struct_def.identity.to_definition_type(id))?;
                    }
                }
                ir::TypeDef::Variant(variant_def) => {
                    if !variant_def.is_generic() {
                        self.add_variant(id, variant_def)?;
                    }
                }
                ir::TypeDef::Function(_func_def) => {
                    // functions don't need special marshaling, we only marshal pointers to them
                }
            };
        }

        for (iface_id, _) in metadata.interfaces() {
            self.add_iface(iface_id)?;
        }

        for (ty, _) in metadata.type_info() {
            if ty.is_object() {
                self.register_object_type(ty.clone())?;
            }
        }

        Ok(())
    }

    pub fn variant_tag_type(&self) -> NativeType {
        NativeType::i32()
    }
    
    fn register_type(&mut self, ty: ir::Type, ffi_ty: NativeType) -> MarshalResult<TypeIndex> {
        if let Some(type_index) = self.type_indices.get_by_right(&ty) {
            return Ok(*type_index);
        }

        let type_index = self.next_type_index;
        self.next_type_index.0 += 1;

        self.type_indices.insert(type_index, ty.clone());
        self.types.insert(type_index, ffi_ty);

        let object_id = match &ty {
            ir::Type::WeakObject(id) | ir::Type::Object(id) => match id {
                | ir::ObjectID::Class(class_id) => {
                    // TODO: class type args
                    let struct_type = class_id.to_struct_type([]);
                    let (_, struct_index) = &self.add_struct_type(&struct_type)?;

                    Some(ObjectID::Struct(*struct_index))
                },
                ir::ObjectID::Array(element) => {
                    Some(ObjectID::Array(element.clone()))
                },
                ir::ObjectID::Box(value) => {
                    Some(ObjectID::Box(value.clone()))
                },
                _ => None,
            },

            _ => None,
        };
 
        if let Some(object_id) = object_id {
            self.object_id_indices.insert(type_index, object_id);
        } else {
            // register the box type for non-object types so all known value types also
            // have a boxed version if we need to use them via RTTI
            self.register_object_type(ty.clone().boxed())?;
        }

        Ok(type_index)
    }

    pub fn register_object_type(&mut self, ty: ir::Type) -> MarshalResult<TypeIndex> {
        if let Some(ir::ObjectID::Class(class_id)) = ty.as_object() {
            // TODO: class type args
            let class_struct_type = class_id.to_struct_type([]);

            let (_, index) = self.add_struct_type(&class_struct_type)?;
            Ok(index)
        } else {
            self.register_type(ty, NativeType::pointer())
        }
    }

    fn add_dyn_array_type(&mut self, element_type: ir::Type) -> MarshalResult<TypeIndex> {
        self.register_type(element_type.clone().dyn_array(), NativeType::pointer())
    }

    fn get_cached_type(&self, t: &ir::Type) -> Option<(NativeType, TypeIndex)> {
        let type_index = self.type_indices.get_by_right(t)?;
        let cached = self.types.get(type_index)?;

        Some((cached.clone(), *type_index))
    }

    pub fn define_struct(
        &mut self,
        struct_type: &ir::Type,
        def: Rc<ir::StructDef>,
    ) -> MarshalResult<(NativeType, TypeIndex)> {
        if let Some(cached) = self.get_cached_type(struct_type) {
            return Ok(cached);
        }

        assert!(matches!(struct_type, ir::Type::Struct {..} | ir::Type::Flags(..)), "not a struct: {}", struct_type);

        let mut def_fields: Vec<_> = def.fields.iter().collect();
        def_fields.sort_by_key(|(id, _)| **id);

        // definitions may have gaps in their field lists, in which case we need to remember
        // the element index of each field so we can find it without expecting the field ID to match
        
        let mut element_count = def_fields.len();
        if def.identity.is_ref_type() {
            element_count += RC_ELEMENT_COUNT;
        }

        let mut field_native_types: Vec<NativeType> = Vec::with_capacity(element_count);
        let mut field_layout = BTreeMap::new();
        
        let mut offset = field_native_types.iter()
            .map(|ty| ty.size())
            .sum::<usize>();

        for (field_id, field_def) in def_fields {
            let foreign_ty = self.build_marshalled_type(&field_def.ty)?;
            field_native_types.push(foreign_ty.clone());
            
            let size = foreign_ty.size();

            field_layout.insert(*field_id, StructFieldInfo {
                offset,
                ty: field_def.ty.clone(),
                foreign_ty,
            });

            offset += size;
        }

        self.add_dyn_array_type(struct_type.clone())?;

        let native_type = NativeType::structure(field_native_types);
        let type_index = self.register_type(struct_type.clone(), native_type.clone())?;

        self.struct_layouts.insert(type_index, StructLayout {
            fields: field_layout,
            def,
        });

        // eprintln!("add_struct: {}={} (size {})", type_index.0, struct_type.to_pretty_string(&self.metadata), native_type.size());

        Ok((native_type, type_index))
    }

    pub fn add_variant(
        &mut self,
        id: ir::TypeDefID,
        def: &ir::VariantDef,
    ) -> MarshalResult<(NativeType, TypeIndex)> {
        let variant_type = ir::Type::Variant(id);

        if let Some(cached) = self.get_cached_type(&variant_type) {
            return Ok(cached);
        }
        let case_tys: Vec<_> = def.cases.iter().map(|case| case.ty.clone()).collect();

        let mut case_ffi_tys = Vec::with_capacity(case_tys.len());
        let mut max_case_size = 0;

        for case_ty in &case_tys {
            if let Some(case_ty) = case_ty.as_ref() {
                let case_ffi_ty = self.build_marshalled_type(case_ty)?;

                max_case_size = max(case_ffi_ty.size(), max_case_size);

                case_ffi_tys.push(case_ffi_ty);
            };
        }

        let variant_layout: Vec<_> = iter::once(self.variant_tag_type())
            .chain(iter::repeat(NativeType::u8()).take(max_case_size))
            .collect();

        let variant_ffi_ty = NativeType::structure(variant_layout);

        self.add_dyn_array_type(variant_type.clone())?;
        let type_index = self.register_type(variant_type, variant_ffi_ty.clone())?;

        self.variant_case_types.insert(type_index, case_tys);

        Ok((variant_ffi_ty, type_index))
    }

    pub fn add_iface(&mut self, id: ir::InterfaceID) -> MarshalResult<TypeIndex> {
        let iface_ty = id.to_interface_ptr_type();
        
        self.add_dyn_array_type(iface_ty)
    }
    
    pub fn add_flags_type(&mut self, id: ir::TypeDefID) -> MarshalResult<NativeType> {
        let flags_type = id.to_flags_type();
        let (_, type_index) = self.add_struct_type(&flags_type)?;

        let native_type = self.types[&type_index].clone();
        Ok(native_type)
    }

    pub fn build_ffi_invoker(
        &mut self,
        func_ref: &ir::ExternalFunctionRef,
    ) -> MarshalResult<FfiInvoker> {
        // the "nothing" type is usually not allowed by the marshaller because it can't be
        // instantiated, but here we need to map it to the void ffi type
        let ffi_return_ty = match &func_ref.sig.return_ty {
            ir::Type::Nothing => NativeType(FfiType::void()),
            return_ty => self.build_marshalled_type(return_ty)?,
        };

        let ffi_param_tys: Vec<_> = func_ref
            .sig
            .param_tys
            .iter()
            .map(|ty| self.build_marshalled_type(ty))
            .collect::<MarshalResult<_>>()?;

        let cif = FfiBuilder::new()
            .args(ffi_param_tys.iter().map(|t| t.0.clone()))
            .res(ffi_return_ty.0.clone())
            .into_cif();

        let lib_filename = format!("{}{}{}", env::consts::DLL_PREFIX, func_ref.src, env::consts::DLL_SUFFIX);
        let lib_path = match env::current_dir() {
            Ok(cwd) => cwd.join(lib_filename),
            Err(..) => PathBuf::from(lib_filename),
        };

        let sym_load_err = |err: DlopenError| MarshalError::ExternSymbolLoadFailed {
            lib: func_ref.src.clone(),
            symbol: func_ref.symbol.clone(),
            path: lib_path.clone(),
            #[allow(deprecated)]
            msg: err.description().to_string(),
            cause: err.source().map(|e| e.to_string())
        };

        let lib = match self.libs.get(&func_ref.src) {
            Some(lib_rc) => lib_rc.clone(),
            None => {
                let lib = dlopen::Library::open(&lib_path)
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
    ) -> MarshalResult<NativeType> {
        if let Some(type_index) = self.try_get_type_index(ty) {
            let cached = self.types[&type_index].clone();
            return Ok(cached);
        }

        match ty {
            ir::Type::Variant(id) => {
                let def = self.metadata
                    .get_variant_def(*id)
                    .cloned()
                    .ok_or_else(|| {
                        MarshalError::unsupported_type(ty.clone())
                    })?;

                let (marshalled_type, _) = self.add_variant(*id, &def)?;
                Ok(marshalled_type)
            },

            ir::Type::Struct { .. } => {
                let (_, type_index) = self.add_struct_type(ty)?;
                let marshalled_type = self.types[&type_index].clone();
                Ok(marshalled_type)
            },

            ir::Type::Object(..) 
            | ir::Type::WeakObject(..) 
            | ir::Type::Pointer(..) 
            | ir::Type::TempRef(..) 
            | ir::Type::Function(..) => {
                let pointer_type = NativeType::pointer();
                
                self.add_dyn_array_type(ty.clone())?;
                self.register_type(ty.clone(), pointer_type.clone())?;
                
                Ok(pointer_type)
            },

            ir::Type::Array { element, dim } => {
                let el_ty = self.build_marshalled_type(&element)?;
                let el_tys = vec![el_ty; *dim];
                
                let array_struct = NativeType::structure(el_tys);

                self.add_dyn_array_type(ty.clone())?;
                self.register_type(ty.clone(), array_struct.clone())?;

                Ok(array_struct)
            },

            ir::Type::Flags(ty_id) => {
                Ok(self.add_flags_type(*ty_id)?)
            }

            // all primitives/builtins should be in the cache already
            _ => {
                Err(MarshalError::unsupported_type(ty.clone()))
            },
        }
    }

    pub fn try_get_type_index(&self, t: &ir::Type) -> Option<TypeIndex> {
        self.type_indices
            .get_by_right(t)
            .cloned()
    }

    pub fn get_type_index(&self, t: &ir::Type) -> MarshalResult<TypeIndex> {
        self.try_get_type_index(t).ok_or_else(|| MarshalError::unsupported_type(t.clone()))
    }

    pub fn get_type(&self, index: TypeIndex) -> MarshalResult<&ir::Type> {
        self.type_indices
            .get_by_left(&index)
            .ok_or_else(|| MarshalError::invalid_type_index(index))
    }

    // TODO: should probably change this to take a TypeIndex
    pub fn get_native_type(&self, ty: &ir::Type) -> MarshalResult<NativeType> {
        match ty {
            ir::Type::Nothing => {
                // "nothing" is not a marshalable type!
                Err(MarshalError::unsupported_type(ir::Type::Nothing))
            },

            // we can't cache array types so always build them on demand
            // pascal static arrays are treated as a struct of elements laid out sequentially
            ir::Type::Array { element, dim } => {
                let el_ty = self.get_native_type(&element)?;
                let el_tys = vec![el_ty; *dim];

                Ok(NativeType::structure(el_tys))
            },

            ir::Type::Pointer(..) 
            | ir::Type::TempRef(..) 
            | ir::Type::Object(..) 
            | ir::Type::WeakObject(..) 
            | ir::Type::Function(..) => {
                Ok(NativeType::pointer())
            },

            ty => {
                let type_index = self.get_type_index(ty)?;
                Ok(self.types[&type_index].clone())
            },
        }
    }

    pub fn get_field_info(&self, struct_type: &ir::Type, field: ir::FieldID) -> MarshalResult<&StructFieldInfo> {
        let type_index = self.get_type_index(struct_type)?;

        let struct_layout = self.struct_layouts
            .get(&type_index)
            .ok_or_else(|| {
                MarshalError::unsupported_type(struct_type.clone())
            })?;

        let field_info = struct_layout.fields
            .get(&field)
            .ok_or_else(|| {
                MarshalError::FieldOutOfRange { struct_type: struct_type.clone(), field }
            })?;

        Ok(field_info)
    }

    pub fn add_struct_type(
        &mut self,
        struct_type: &ir::Type,
    ) -> MarshalResult<(Rc<ir::StructDef>, TypeIndex)> {
        if let Some(type_index) = self.try_get_type_index(struct_type)
            && let Some(layout) = self.struct_layouts.get(&type_index)
        {
            return Ok((layout.def.clone(), type_index));
        }

        let (def_id, args): (_, &[ir::Type]) = match struct_type {
            ir::Type::Struct { id, args } => (*id, args),
            ir::Type::Flags(id) => (*id, &[]),

            // for object types that are pointers to an inner struct, instantiate that inner
            // struct and register the object type separately
            ir::Type::Object(ir::ObjectID::Class(..))
            | ir::Type::WeakObject(ir::ObjectID::Class(..)) => {
                let type_index = self.register_type(struct_type.clone(), NativeType::pointer())?;

                let def = match self.object_id_indices.get_by_left(&type_index) {
                    Some(ObjectID::Struct(struct_index)) => {
                        self.struct_layouts[&struct_index].def.clone()
                    },
                    _ => unreachable!("{} must have a registered struct object ID after register_object_type was called", struct_type),
                };

                return Ok((def, type_index));
            }

            _ => {
                return Err(MarshalError::unsupported_type(struct_type.clone()));
            }
        };

        let generic_def = self
            .metadata()
            .get_struct_def(def_id)
            .cloned()
            .ok_or_else(|| {
                MarshalError::MissingTypeDef(struct_type.clone())
            })?;

        match instantiate_struct_def(&generic_def, args) {
            Cow::Borrowed(..) => {
                // not generic
                let def = Rc::new(generic_def);
                let (_, type_index) = self.define_struct(struct_type, def.clone())?;

                Ok((def, type_index))
            },

            Cow::Owned(new_def) => {
                if self.trace_generics {
                    let new_name = new_def.identity.to_pretty_string(self.metadata().as_formatter());
                    eprintln!("[vm] new instantiation of struct {}: {}", def_id, new_name);
                }

                let def = Rc::new(new_def);
                let (_, struct_index) = self.define_struct(struct_type, def.clone())?;

                Ok((def, struct_index))
            },
        }
    }

    pub fn marshal(&self, val: &DynValue, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let size = match val {
            DynValue::I8(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::U8(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::I16(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::U16(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::I32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::U32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::I64(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::U64(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::ISize(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::USize(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::F32(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
            DynValue::F64(x) => marshal_bytes(&x.to_ne_bytes(), out_bytes)?,
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
                marshal_bytes(&func_id.0.to_ne_bytes(), out_bytes)?
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
                let struct_type = self.get_type(structure.type_index)?;
                let ty = self.get_native_type(&struct_type)?;
                ty.size()
            }

            DynValue::Variant(variant) => {
                let variant_type = self.get_type(variant.type_index)?;
                let ty = self.get_native_type(&variant_type)?;
                ty.size()
            }

            DynValue::Array(array) => {
                let el_size = self.get_native_type(&array.element_type)?.size();
                array.elements.len() * el_size
            }
        };
        
        let mut bytes = vec![0u8; capacity];
        self.marshal(val, &mut bytes)?;
        
        Ok(bytes)
    } 

    fn marshal_ptr(&self, ptr: &Pointer, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let ptr_bytes = ptr.addr.to_ne_bytes();
        let len = marshal_bytes(&ptr_bytes, out_bytes)?;
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
        assert_ne!(0, pointer.addr);

        let marshal_ty = self.get_native_type(&pointer.ty)?;

        let result = self.unmarshal(unsafe {
            pointer.as_slice(marshal_ty.size())
        }, &pointer.ty)?;

        Ok(result.value)
    }

    pub fn marshal_object_at(&self, pointer: &Pointer, object: &ObjectValue) -> MarshalResult<usize> {
        let mut offset = 0;

        offset += unsafe {
            let header_mem = pointer.as_slice_mut(Self::object_header_size());
            self.marshal_object_header(&object.header, header_mem)?
        };

        match (&object.header.id, &object.value) {
            (ObjectID::Box(value_type), boxed_val) => {
                let body_ptr = Pointer::new(pointer.addr + offset, value_type.as_ref().clone());
                offset += self.marshal_at(boxed_val, &body_ptr)?;
                
                Ok(offset)
            }

            (ObjectID::Struct(type_index), DynValue::Structure(..)) => {
                let struct_type = self.get_type(*type_index)?;
                let struct_ptr = Pointer::new(pointer.addr + offset, struct_type.clone());
                offset += self.marshal_at(&object.value, &struct_ptr)?;
                Ok(offset)
            }

            (
                ObjectID::Array(element),
                DynValue::Array(array_val)
            ) if array_val.element_type == **element => {
                let element_count = array_val.elements.len();

                let size_ptr = Pointer::new(pointer.addr + offset, ir::Type::I32);
                let size = i32::try_from(element_count)
                    .map_err(|_| {
                        MarshalError::unsupported_type(element.as_ref().clone().array(element_count))
                    })?;
                
                offset += unsafe {
                    let size_mem = size_ptr.as_slice_mut(NativeType::i32().size());
                    marshal_bytes(&size.to_ne_bytes(), size_mem)?
                };

                let elements_ty = element.as_ref().clone().array(element_count);
                let elements_ptr = Pointer::new(pointer.addr + offset, elements_ty);

                offset += self.marshal_at(&object.value, &elements_ptr)?;
                
                Ok(offset)
            }

            _ => {
                Err(MarshalError::InvalidData)
            }
        }
    }

    pub fn unmarshal_object_at(&self, pointer: &Pointer) -> MarshalResult<ObjectValue> {
        assert_ne!(0, pointer.addr);
        
        let mem = unsafe {
            pointer.as_slice(Self::object_header_size())
        };

        let header = self.unmarshal_object_header(mem)?;
        let body_addr = pointer.addr + header.byte_count;
        
        let value = match &header.value.id {
            ObjectID::Box(value) => {
                let value_ptr = Pointer { addr: body_addr, ty: value.as_ref().clone() };

                self.unmarshal_at(&value_ptr)?
            }

            ObjectID::Struct(type_index) => {
                let struct_type = self.get_type(*type_index)?;
                assert!(matches!(struct_type, ir::Type::Struct { .. }));

                let struct_ptr = Pointer { 
                    addr: body_addr, 
                    ty: struct_type.clone(),
                };
                
                self.unmarshal_at(&struct_ptr)?
            }

            ObjectID::Array(element) => {
                let size_ptr = Pointer { addr: body_addr, ty: ir::Type::I32 };
                let size = unmarshal_from_ne_bytes(unsafe {
                    size_ptr.as_slice(NativeType::i32().size())
                }, i32::from_ne_bytes)?;
                
                let array_dim = usize::try_from(size.value)
                    .map_err(|_| {
                        MarshalError::InvalidData
                    })?;

                let array_ptr = Pointer { 
                    addr: body_addr + size.byte_count, 
                    ty: element.as_ref().clone().array(array_dim) 
                };
                self.unmarshal_at(&array_ptr)?
            }
        };
        
        Ok(ObjectValue {
            header: header.value,
            value,
        })
    }

    pub fn marshal_at(&self, val: &DynValue, into_ptr: &Pointer) -> MarshalResult<usize> {
        if into_ptr.addr == 0 {
            return Err(MarshalError::InvalidData);
        }
        
        // eprintln!("marshal_at: {into_ptr} <- {:?}", val);

        let marshal_ty = self.get_native_type(&into_ptr.ty)?;
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

            ir::Type::Object(..)
            | ir::Type::WeakObject(..) => {
                self.unmarshal_ptr(ir::Type::Nothing, in_bytes)?
                    .map(DynValue::Pointer)
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
            ir::Type::Struct { .. } | ir::Type::Flags { .. } => {
                let type_index = self.get_type_index(ty)?;
                let struct_val = self.unmarshal_struct(type_index, in_bytes)?;

                UnmarshalledValue {
                    value: DynValue::Structure(Box::new(struct_val.value)),
                    byte_count: struct_val.byte_count,
                }
            },

            ir::Type::Variant(..) => {
                let type_index = self.get_type_index(ty)?;
                let variant_val = self.unmarshal_variant(type_index, in_bytes)?;
                variant_val.map(Box::new).map(DynValue::Variant)
            },

            _ => {
                return Err(MarshalError::unsupported_type(ty.clone()));
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

                let ty_size = self.get_native_type(ty)?.size();
                let local_size = &mut local_sizes[id.0];
                *local_size = max(ty_size, *local_size);
            }
        }

        let size_sum = local_sizes.into_iter().sum();
        Ok(size_sum)
    }

    pub fn marshal_object_header(
        &self,
        header: &ObjectHeader,
        out_bytes: &mut [u8],
    ) -> MarshalResult<usize> {
        let id_index = *self.object_id_indices
            .get_by_right(&header.id)
            .ok_or_else(|| {
                match header.id.to_type(self) {
                    Ok(object_type) => MarshalError::unsupported_type(object_type),
                    Err(err) => err,
                }
            })?;

        let mut offset = 0;
        offset += marshal_bytes(&id_index.0.to_ne_bytes(), &mut out_bytes[offset..])?;
        offset += marshal_bytes(&header.strong_count.to_ne_bytes(), &mut out_bytes[offset..])?;
        offset += marshal_bytes(&header.weak_count.to_ne_bytes(), &mut out_bytes[offset..])?;

        Ok(offset)
    }
    
    pub fn marshal_array_object_header(&self,
        header: &ObjectHeader,
        count: usize,
        out_bytes: &mut [u8],
    ) -> MarshalResult<usize> {
        let mut offset = self.marshal_object_header(header, out_bytes)?; 
        
        let element_ty = match &header.id {
            ObjectID::Array(element_ty) => element_ty.clone(),
            _ => {
                // wrong header for an array
                return Err(MarshalError::InvalidData);
            }
        };

        let size = i32::try_from(count)
            .map_err(|_| {
                MarshalError::unsupported_type(element_ty.as_ref().clone().array(count))
            })?;

        offset += marshal_bytes(&size.to_ne_bytes(), &mut out_bytes[offset..])?;
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
        
        let object_id = self.object_id_indices
            .get_by_left(&TypeIndex(type_index.value))
            .ok_or_else(|| {
                MarshalError::invalid_type_index(TypeIndex(type_index.value))
            })?;

        Ok(UnmarshalledValue {
            value: ObjectHeader {
                id: object_id.clone(),
                strong_count: strong_count.value,
                weak_count: weak_count.value,
            },
            byte_count: offset,
        })
    }

    pub fn object_header_size() -> usize {
        NativeType::u64().size() // type index
            + NativeType::i32().size() // strong ref count
            + NativeType::i32().size() // weak ref count
    }
    
    // the array header is the region of memory preceding the elements of a dynamic array
    pub fn array_header_size() -> usize {
        let header_size = Self::object_header_size()
            + NativeType::i32().size(); // element count
        
        header_size
    }
    
    fn unmarshal_dyn_array_header(&self, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<DynArrayHeader>> {
        let mut offset = 0;

        // non-static arrays must be RC objects
        let header = self.unmarshal_object_header(&in_bytes[offset..])?;
        offset += header.byte_count;

        assert!(matches!(header.value.id, ObjectID::Array(..)));

        let size_int = unmarshal_from_ne_bytes(&in_bytes[offset..], i32::from_ne_bytes)?;
        offset += size_int.byte_count;
        
        Ok(UnmarshalledValue {
            value: DynArrayHeader {
                object_header: header.value,
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
            },
        })
    }

    fn marshal_struct(&self, struct_val: &StructValue, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let mut offset = 0;
        
        let Some(layout) = self.struct_layouts.get(&struct_val.type_index) else {
            // struct type is not in metadata
            let struct_type = self.get_type(struct_val.type_index)?;
            return Err(MarshalError::InvalidStructType { ty: struct_type.clone() });
        };

        for (field_id, field_info) in &layout.fields {
            match struct_val.fields.get(field_id.0) {
                Some(field_val) => {
                    offset += self.marshal(field_val, &mut out_bytes[offset..])?;
                }
                
                // skip this field
                None => {
                    let field_foreign_ty = self.get_native_type(&field_info.ty)?;
                    offset += field_foreign_ty.size();
                }
            };
        }

        Ok(offset)
    }

    fn unmarshal_struct(&self, type_index: TypeIndex, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<StructValue>> {
        let mut offset = 0;

        let layout = self.struct_layouts
            .get(&type_index)
            .ok_or_else(|| MarshalError::invalid_type_index(type_index))?;

        let fields_len = layout.fields.keys()
            .map(|id| id.0)
            .max()
            .map(|max_id| max_id + 1)
            .unwrap_or(0);

        let mut fields = vec![DynValue::I32(-1); fields_len];

        for (id, field_info) in &layout.fields {
            let field_val = self.unmarshal(&in_bytes[offset..], &field_info.ty)?;
            offset += field_val.byte_count;
            fields[id.0] = field_val.value;
        }

        Ok(UnmarshalledValue {
            byte_count: offset,
            value: StructValue {
                fields,
                type_index,
            },
        })
    }

    fn marshal_variant(
        &self,
        variant_val: &VariantValue,
        out_bytes: &mut [u8],
    ) -> MarshalResult<usize> {
        let variant_type = self.get_type(variant_val.type_index)?;

        let case_tys = self
            .variant_case_types
            .get(&variant_val.type_index)
            .ok_or_else(|| {
                MarshalError::unsupported_type(variant_type.clone())
            })?;

        let tag = variant_val
            .tag
            .as_i32()
            .ok_or_else(|| MarshalError::InvalidData)?;

        let case_ty = case_tys
            .get(tag as usize)
            .ok_or_else(|| MarshalError::InvalidData)?;

        // we still need to refer to the type size, because we always marshal/unmarshal
        // the entire size of the variant regardless of which case is active
        let variant_size = self.get_native_type(&variant_type)?.size();

        let tag_size = self.marshal(&variant_val.tag, out_bytes)?;

        // don't marshal anything for data-less cases
        let data_size = match case_ty {
            None => 0,
            Some(case_ty) => {
                let data_size = self.marshal(&variant_val.data, &mut out_bytes[tag_size..])?;
                let ty_size = self.get_native_type(case_ty)?.size();
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
            variant_type,
        );

        Ok(variant_size)
    }

    fn unmarshal_variant(
        &self,
        type_index: TypeIndex,
        in_bytes: &[u8],
    ) -> MarshalResult<UnmarshalledValue<VariantValue>> {
        let tag_val = self.unmarshal(in_bytes, &ir::Type::I32)?;
        let tag = tag_val.value.as_i32().unwrap();

        let variant_type = self.get_type(type_index)?;

        let case_index = cast::usize(tag).map_err(|_| MarshalError::VariantTagOutOfRange {
            variant_type: variant_type.clone(),
            tag: tag_val.value.clone(),
        })?;

        let variant_size = self.get_native_type(&variant_type)?.size();

        let case_tys = self
            .variant_case_types
            .get(&type_index)
            .ok_or_else(|| {
                MarshalError::unsupported_type(variant_type.clone())
            })?;

        let case_ty = case_tys
            .get(case_index)
            .ok_or_else(|| MarshalError::VariantTagOutOfRange {
                variant_type: variant_type.clone(),
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
                type_index,
                tag: Box::new(DynValue::I32(tag)),
                data: Box::new(data_val.value),
            },
        })
    }
}

fn marshal_bytes(bytes: &[u8], out_bytes: &mut [u8]) -> MarshalResult<usize> {
    if bytes.len() > out_bytes.len() {
        return Err(MarshalError::InvalidWrite {
            data_size: bytes.len(),
            dest_size: out_bytes.len(),
        });
    }
    
    out_bytes[0..bytes.len()].copy_from_slice(bytes);
    Ok(bytes.len())
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