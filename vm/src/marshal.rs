mod util;
mod error;
mod native_type;
mod type_def;
mod variant_type;
mod struct_type;
mod array;
mod object;

pub(crate) use self::array::*;
pub(crate) use self::struct_type::*;
use self::util::unmarshal_from_ne_bytes;
use self::util::UnmarshalledValue;
pub(crate) use self::variant_type::*;
use crate::func::ffi::FfiInvoker;
use crate::func::FuncInstanceID;
use crate::func::Function;
use crate::ir;
use crate::DynValue;
use crate::ObjectID;
use crate::Pointer;
use bimap::BiHashMap;
use ::dlopen::raw as dlopen;
use ::dlopen::Error as DlopenError;
pub use error::*;
use ir::MetadataSource as _;
use libffi::middle::Builder as FfiBuilder;
use libffi::middle::Type as FfiType;
pub use native_type::*;
use std::cmp::max;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::convert::TryInto;
use std::env;
use std::error::Error;
use std::mem::size_of;
use std::path::PathBuf;
use std::ptr::slice_from_raw_parts_mut;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Marshaller {
    metadata: ir::Metadata,

    types: BTreeMap<TypeID, NativeType>,
    libs: HashMap<String, Rc<dlopen::Library>>,

    struct_layouts: BTreeMap<TypeID, StructLayout>,
    variant_layouts: BTreeMap<TypeID, VariantLayout>,

    // map of known object IDs to serializable indices
    next_type_index: TypeID,
    type_indices: BiHashMap<TypeID, ir::Type>,
    object_id_indices: BiHashMap<TypeID, ObjectID>,

    object_dtors: BTreeMap<TypeID, Rc<Function>>,

    func_instances: BiHashMap<ir::FunctionRef, FuncInstanceID>,

    trace_generics: bool,
}

impl Marshaller {
    pub fn new(metadata: ir::Metadata, trace_generics: bool) -> MarshalResult<Self> {
        let mut marshaller = Self {
            metadata,

            types: BTreeMap::new(),
            libs: HashMap::new(),

            struct_layouts: BTreeMap::new(),
            variant_layouts: BTreeMap::new(),
            
            next_type_index: TypeID(0),
            object_id_indices: BiHashMap::new(),
            type_indices: BiHashMap::new(),

            func_instances: BiHashMap::new(),

            object_dtors: BTreeMap::new(),

            trace_generics,
        };

        let primitive_types = [
            (ir::Type::Nothing, NativeType::void()),
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
            (ir::Type::string(), NativeType::pointer()),
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
                        let struct_type = struct_def.identity.to_definition_type(id);
                        self.add_struct_type(&struct_type)?;
                    }
                }
                ir::TypeDef::Variant(variant_def) => {
                    if !variant_def.is_generic() {
                        let variant_type = id.to_variant_type(variant_def.name.type_args.clone());
                        self.add_variant_type(&variant_type)?;
                    }
                }
            };
        }

        for (iface_id, iface_def) in metadata.interface_defs() {
            let iface_ref = ir::InterfaceRef::new(iface_id, iface_def.name.type_args.clone());
            self.add_iface_type(&iface_ref)?;
        }

        for (ty, _) in metadata.type_info() {
            if let Some(ir::ObjectID::Class(class_id)) = ty.as_object() {
                let def = self.metadata
                    .get_struct_def(class_id.def_id)
                    .ok_or_else(|| {
                        MarshalError::MissingTypeDef(ty.clone())
                    })?;

                if !def.is_generic() {
                    self.add_struct_type(&class_id.to_class_object_type())?;
                }
            } else if ty.is_object() {
                self.register_object_type(ty.clone())?;
            }
        }

        Ok(())
    }

    fn register_type(&mut self, ty: ir::Type, native_type: NativeType) -> MarshalResult<TypeID> {
        if let Some(type_index) = self.type_indices.get_by_right(&ty) {
            return Ok(*type_index);
        }

        let type_index = self.next_type_index;
        self.next_type_index.0 += 1;

        // eprintln!("size of {} ({}) is {}", ty.to_pretty_string(self.metadata()), ty, native_type.size());

        self.type_indices.insert(type_index, ty.clone());
        self.types.insert(type_index, native_type);

        match &ty {
            ir::Type::WeakObject(id) | ir::Type::Object(id) => {
                // for any object type, first register the strong type (might be the same as
                // type index, or not if we register the weak type first)
                let strong_index = self.register_object_type(id.to_object_type())?;

                match id {
                    ir::ObjectID::Class(class_id) => {
                        let struct_type = class_id.to_class_object_type();
                        self.add_struct_type(&struct_type)?;
                    },

                    ir::ObjectID::Array(element) => {
                        let (element_id, _) = self.build_marshalled_type(element)?;
                        let object_id = ObjectID::Array(element_id);
                        self.object_id_indices.insert(strong_index, object_id.clone());

                        self.gen_array_dtor(&element)?;
                    },

                    ir::ObjectID::Box(value) => {
                        let (value_id, _) = self.build_marshalled_type(value)?;
                        let object_id = ObjectID::Box(value_id);
                        self.object_id_indices.insert(strong_index, object_id.clone());

                        self.gen_box_dtor(&value)?;
                    },

                    _ => {
                        // abstract
                    },
                };

                self.register_object_type(id.to_weak_object_type())?;
            },

            _ => {
                // register the box type for non-object types so all known value types also
                // have a boxed version if we need to use them via RTTI
                self.register_object_type(ty.clone().boxed())?;
            },
        };

        Ok(type_index)
    }

    pub fn register_object_type(&mut self, ty: ir::Type) -> MarshalResult<TypeID> {
        self.register_type(ty, NativeType::pointer())
    }

    fn get_cached_type(&self, t: &ir::Type) -> Option<(NativeType, TypeID)> {
        let type_index = self.type_indices.get_by_right(t)?;
        let cached = self.types.get(type_index)?;

        Some((cached.clone(), *type_index))
    }

    pub fn add_iface_type(&mut self, id: &ir::InterfaceRef) -> MarshalResult<TypeID> {
        let iface_ty = id.to_interface_type();

        self.add_dyn_array_type(iface_ty)
    }

    pub fn build_ffi_invoker(
        &mut self,
        func_ref: &ir::ExternalFunctionRef,
    ) -> MarshalResult<FfiInvoker> {
        // the "nothing" type is usually not allowed by the marshaller because it can't be
        // instantiated, but here we need to map it to the void ffi type
        let native_return_type = match &func_ref.sig.result_type {
            ir::Type::Nothing => NativeType(FfiType::void()),
            return_ty => {
                let (_, native_type) = self.build_marshalled_type(return_ty)?;
                native_type
            },
        };

        let native_param_types: Vec<_> = func_ref
            .sig
            .param_types
            .iter()
            .map(|ty| {
                let (_, native_type) = self.build_marshalled_type(ty)?;
                Ok(native_type)
            })
            .collect::<MarshalResult<_>>()?;

        let cif = FfiBuilder::new()
            .args(native_param_types.iter().map(|t| t.0.clone()))
            .res(native_return_type.0.clone())
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
            native_param_types,
            func_ref.sig.result_type.clone(),
            native_return_type,
        ))
    }

    fn build_marshalled_type(
        &mut self,
        ty: &ir::Type,
    ) -> MarshalResult<(TypeID, NativeType)> {
        if let Some(id) = self.try_get_type_index(ty) {
            let cached = self.types[&id].clone();
            return Ok((id, cached));
        }

        match ty {
            ir::Type::Variant(..) => {
                let (_, type_index) = self.add_variant_type(ty)?;
                let marshalled_type = self.types[&type_index].clone();
                Ok((type_index, marshalled_type))
            },

            ir::Type::Struct { .. } => {
                let (_, type_index) = self.add_struct_type(ty)?;
                let marshalled_type = self.types[&type_index].clone();
                Ok((type_index, marshalled_type))
            },

            ir::Type::Object(..) 
            | ir::Type::WeakObject(..) 
            | ir::Type::Pointer(..) 
            | ir::Type::TempRef(..) 
            | ir::Type::Function(..) => {
                let pointer_type = NativeType::pointer();
                
                self.add_dyn_array_type(ty.clone())?;
                let id = self.register_type(ty.clone(), pointer_type.clone())?;
                
                Ok((id, pointer_type))
            },

            ir::Type::Array { element, dim } => {
                let (_, el_ty) = self.build_marshalled_type(&element)?;
                let el_tys = vec![el_ty; *dim];
                
                let array_struct = NativeType::structure(el_tys);

                self.add_dyn_array_type(ty.clone())?;
                let id = self.register_type(ty.clone(), array_struct.clone())?;

                Ok((id, array_struct))
            },

            ir::Type::Generic(..) => {
                let type_id = self.register_type(ty.clone(), NativeType::void())?;
                Ok((type_id, NativeType::void()))
            }

            // all primitives/builtins should be in the cache already
            _ => {
                Err(MarshalError::unsupported_type(ty.clone()))
            },
        }
    }

    pub fn try_get_type_index(&self, t: &ir::Type) -> Option<TypeID> {
        self.type_indices
            .get_by_right(t)
            .cloned()
    }

    pub fn get_type_index(&self, t: &ir::Type) -> MarshalResult<TypeID> {
        self.try_get_type_index(t).ok_or_else(|| MarshalError::unsupported_type(t.clone()))
    }

    pub fn get_type(&self, index: TypeID) -> MarshalResult<&ir::Type> {
        self.type_indices
            .get_by_left(&index)
            .ok_or_else(|| MarshalError::invalid_type_index(index))
    }

    // get or create a native type corresponding to a type referenced in code.
    // for non-generic type, this is just a lookup to the corresponding type defined in metadata.
    // for generic types, this may instantiate a new instance of the generic definition if
    // necessary, creating a new native layout with a unique type index
    pub fn create_native_type(&mut self, ty: &ir::Type) -> MarshalResult<NativeType> {
        if let Some(type_index) = self.try_get_type_index(ty) {
            let native_type = self.types[&type_index].clone();
            return Ok(native_type);
        }

        match ty {
            ir::Type::Nothing => {
                // "nothing" is not a marshalable type!
                Err(MarshalError::unsupported_type(ir::Type::Nothing))
            },

            // static arrays are treated as a struct of elements laid out sequentially
            ir::Type::Array { element, dim } => {
                let el_ty = self.create_native_type(&element)?;
                let el_tys = vec![el_ty; *dim];

                let native_type = NativeType::structure(el_tys);

                self.register_type(ty.clone(), native_type.clone())?;

                Ok(native_type)
            },

            // struct types (potentially generic)
            ir::Type::Struct(..) => {
                let (_, type_index) = self.add_struct_type(ty)?;
                let native_type = self.types[&type_index].clone();
                Ok(native_type)
            }

            // variant types (potentially generic)
            ir::Type::Variant(..) => {
                let (_, type_index) = self.add_variant_type(ty)?;
                let native_type = self.types[&type_index].clone();
                Ok(native_type)
            }

            // object pointers to classes
            ir::Type::Object(ir::ObjectID::Class(..))
            | ir::Type::WeakObject(ir::ObjectID::Class(..)) => {
                // instantiate the generic type if necessary
                self.add_struct_type(ty)?;
                Ok(NativeType::pointer())
            }

            // simple pointer types
            ir::Type::Pointer(..)
            | ir::Type::TempRef(..)
            | ir::Type::Object(..)
            | ir::Type::WeakObject(..)
            | ir::Type::Function(..) => {
                self.register_type(ty.clone(), NativeType::pointer())?;
                Ok(NativeType::pointer())
            }

            ir::Type::Generic(..) => {
                self.register_type(ty.clone(), NativeType::void())?;
                Ok(NativeType::void())
            }

            // all other types must exist in the cache already
            ty => {
                let type_index = self.get_type_index(ty)?;
                let native_type = self.types[&type_index].clone();
                Ok(native_type)
            },
        }
    }

    // TODO: should probably change this to take a TypeIndex
    pub fn get_native_type(&self, type_index: TypeID) -> MarshalResult<&NativeType> {
        self.types
            .get(&type_index)
            .ok_or_else(|| {
                MarshalError::invalid_type_index(type_index)
            })
    }

    pub fn get_object_id(&self, index: TypeID) -> MarshalResult<&ObjectID> {
        match self.object_id_indices.get_by_left(&index) {
            Some(id) => Ok(id),
            None => {
                let object_type = self.get_type(index)?;
                Err(MarshalError::InvalidObjectType(object_type.clone()))
            }
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

    pub fn marshal(&mut self, val: &DynValue, out_bytes: &mut [u8]) -> MarshalResult<usize> {
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

            DynValue::Variant(variant_val) => {
                self.marshal_variant(variant_val, out_bytes)?
            },

            DynValue::Pointer(ptr) => {
                self.marshal_ptr(ptr, out_bytes)?
            },

            DynValue::Function(key) => {
                // valid function values can only be created by marshaling them here, so we
                // can register the instance IDs as they're marshaled and expect there will be
                // no attempts to unmarshal the same function before this
                let id = match self.func_instances.get_by_left(key) {
                    Some(key) => *key,
                    None => {
                        let id = FuncInstanceID(self.func_instances.len());
                        self.func_instances.insert(key.clone(), id);

                        id
                    }
                };

                marshal_bytes(&id.0.to_ne_bytes(), out_bytes)?
            }
        };

        Ok(size)
    }
    
    pub fn marshal_to_vec(&mut self, val: &DynValue) -> MarshalResult<Vec<u8>> {
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
                let ty = self.get_native_type(structure.type_index)?;
                ty.size()
            }

            DynValue::Variant(variant) => {
                let ty = self.get_native_type(variant.type_index)?;
                ty.size()
            }

            DynValue::Array(array) => {
                let el_size = self.create_native_type(&array.element_type)?.size();

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

    pub fn unmarshal_at(&self, pointer: &Pointer) -> MarshalResult<DynValue> {
        assert_ne!(0, pointer.addr);

        let type_index = self.get_type_index(&pointer.ty)?;
        let marshal_ty = self.get_native_type(type_index)?;

        let result = self.unmarshal(unsafe {
            pointer.as_slice(marshal_ty.size())
        }, &pointer.ty)?;

        Ok(result.value)
    }

    pub fn marshal_at(&mut self, val: &DynValue, into_ptr: &Pointer) -> MarshalResult<usize> {
        if into_ptr.addr == 0 {
            return Err(MarshalError::InvalidData);
        }
        
        // eprintln!("marshal_at: {into_ptr} <- {:?}", val);

        let marshal_ty = self.create_native_type(&into_ptr.ty)?;
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

                let key = self.func_instances
                    .get_by_right(&FuncInstanceID(func_id.value))
                    .ok_or_else(|| MarshalError::InvalidData)?
                    .clone();

                UnmarshalledValue {
                    value: DynValue::Function(key),
                    byte_count: func_id.byte_count,
                }
            }

            ir::Type::Array { element, dim } => {
                self.unmarshal_static_array(element, *dim, in_bytes)?
                    .map(|array| DynValue::Array(Box::new(array)))
            },

            // these need field offset/tag type info from the ffi cache so marshal/unmarshal should
            // be members
            ir::Type::Struct { .. } => {
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

    pub fn stack_alloc_size(&mut self, instructions: &[ir::Instruction]) -> MarshalResult<usize> {
        let mut local_sizes = Vec::new();
        for instruction in instructions {
            if let ir::Instruction::LocalAlloc(id, ty) = instruction {
                while local_sizes.len() <= id.0 {
                    local_sizes.push(0);
                }

                let ty_size = self.create_native_type(ty)?.size();
                let local_size = &mut local_sizes[id.0];
                *local_size = max(ty_size, *local_size);
            }
        }

        let size_sum = local_sizes.into_iter().sum();
        Ok(size_sum)
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