use crate::ir;
use crate::marshal::util::unmarshal_from_ne_bytes;
use crate::marshal::util::UnmarshalledValue;
use crate::marshal::MarshalError;
use crate::marshal::MarshalResult;
use crate::marshal::Marshaller;
use crate::marshal::NativeType;
use crate::marshal::TypeIndex;
use crate::ArrayValue;
use crate::ObjectHeader;
use crate::ObjectID;
use crate::Pointer;
use std::ptr::slice_from_raw_parts;
use terapascal_ir::InstructionBuilder;

// dynamic array values in memory are marshalled as a set of header fields followed by a variably
// sized sequence of elements
#[derive(Clone, Debug)]
pub struct DynArrayHeader {
    pub object_header: ObjectHeader,
    pub len: i32,
}

impl Marshaller {
    pub(super) fn add_dyn_array_type(&mut self, element_type: ir::Type) -> MarshalResult<TypeIndex> {
        self.register_type(element_type.clone().dyn_array(), NativeType::pointer())
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

    pub(super) fn unmarshal_static_array(&self, element_type: &ir::Type, size: usize, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<ArrayValue>> {
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
    
    pub(super) fn gen_array_dtor(&mut self, element_type: &ir::Type) -> MarshalResult<()> {
        if !element_type.contains_any_object_refs(&self.metadata) {
            return Ok(())
        };

        let array_type = element_type.dyn_array();

        self.gen_runtime_dtor(&array_type, |builder, self_arg| {
            builder.gen_dyn_array_dtor_body(self_arg, element_type);
            true
        })
    }
}