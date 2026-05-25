use crate::func::Function;
use crate::ir;
use crate::marshal::marshal_bytes;
use crate::marshal::util::unmarshal_from_ne_bytes;
use crate::marshal::util::UnmarshalledValue;
use crate::marshal::MarshalError;
use crate::marshal::MarshalResult;
use crate::marshal::Marshaller;
use crate::marshal::NativeType;
use crate::marshal::TypeID;
use crate::DynValue;
use crate::ObjectHeader;
use crate::ObjectID;
use crate::ObjectValue;
use crate::Pointer;
use std::rc::Rc;
use terapascal_ir::InstructionBuilder;

impl Marshaller {
    pub fn marshal_object_at(
        &mut self,
        pointer: &Pointer,
        object: &ObjectValue,
    ) -> MarshalResult<usize> {
        let mut offset = 0;

        offset += unsafe {
            let header_mem = pointer.as_slice_mut(Self::object_header_size());
            self.marshal_object_header(&object.header, header_mem)?
        };

        match (&object.header.id, &object.value) {
            (ObjectID::Box(value_id), boxed_val) => {
                let value_type = self.get_type(*value_id)?.clone();

                let body_ptr = Pointer::new(pointer.addr + offset, value_type);
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
                ObjectID::Array(element_id),
                DynValue::Array(array_val)
            ) => {
                let element_type = self.get_type(*element_id)?.clone();

                if array_val.element_type != element_type {
                    return Err(MarshalError::InvalidData);
                }

                let element_count = array_val.elements.len();
                let element_array_type = element_type.array(element_count);

                let size_ptr = Pointer::new(pointer.addr + offset, ir::Type::I32);
                let size = i32::try_from(element_count)
                    .map_err(|_| {
                        MarshalError::unsupported_type(element_array_type.clone())
                    })?;

                offset += unsafe {
                    let size_mem = size_ptr.as_slice_mut(NativeType::i32().size());
                    marshal_bytes(&size.to_ne_bytes(), size_mem)?
                };

                let elements_ptr = Pointer::new(pointer.addr + offset, element_array_type);

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
            ObjectID::Box(value_id) => {
                let value_type = self.get_type(*value_id)?;
                let value_ptr = Pointer::new(body_addr, value_type.clone());

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

            ObjectID::Array(element_id) => {
                let element_type = self.get_type(*element_id)?;

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
                    ty: element_type.array(array_dim)
                };
                self.unmarshal_at(&array_ptr)?
            }
        };

        Ok(ObjectValue {
            header: header.value,
            value,
        })
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
                    Ok(object_type) => MarshalError::InvalidObjectType(object_type),
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

        let element_type = match &header.id {
            ObjectID::Array(element_id) => {
                self.get_type(*element_id)?.clone()
            },
            _ => {
                // wrong header for an array
                return Err(MarshalError::InvalidData);
            }
        };

        let size = i32::try_from(count)
            .map_err(|_| {
                MarshalError::unsupported_type(element_type.array(count))
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

        let object_id = self.get_object_id(TypeID(type_index.value))?;

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

    pub(super) fn gen_runtime_dtor<F>(
        &mut self,
        object_type: &ir::Type,
        build_fn: F,
    ) -> MarshalResult<()>
    where
        F: FnOnce(&mut ir::RawInstructionBuilder<ir::Metadata>, ir::ArgID) -> bool
    {
        let self_arg_id = ir::ArgID(0);
        let type_index = self.get_type_index(object_type)?;

        let mut builder = ir::RawInstructionBuilder::new(self.metadata(), true);
        builder.local_stack_mut().bind_unnamed_param(self_arg_id, object_type.clone(), false);

        if !build_fn(&mut builder, self_arg_id) {
            return Ok(());
        }

        let dtor_body = builder.finish();
        let dtor_name = format!("<runtime dtor for {}>", object_type.to_pretty_string(&self.metadata));

        let dtor_def = ir::FunctionDef {
            body: dtor_body,
            type_params: Vec::new(),
            debug_name: Some(dtor_name.clone()),
            sig: Rc::new(ir::FunctionSig::new([object_type.clone()], ir::Type::Nothing)),
        };

        self.object_dtors.insert(type_index, Rc::new(Function::new_internal(dtor_name, dtor_def)));

        Ok(())
    }

    pub(super) fn gen_box_dtor(&mut self, value_type: &ir::Type) -> MarshalResult<()> {
        if !value_type.contains_any_object_refs(&self.metadata) {
            return Ok(());
        }

        let box_type = value_type.boxed();

        self.gen_runtime_dtor(&box_type, |builder, self_arg| {
            let value_ref = self_arg.to_ref().element_ref(box_type.clone(), ir::Value::I32_0);
            builder.release(value_ref.to_deref(), value_type.clone(), ir::Ref::Discard);
            true
        })
    }

    pub fn get_runtime_dtor(&self, t: TypeID) -> Option<Rc<Function>> {
        self.object_dtors.get(&t).cloned()
    }
}