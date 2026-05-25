use crate::ir;
use crate::marshal::util::UnmarshalledValue;
use crate::marshal::MarshalError;
use crate::marshal::MarshalResult;
use crate::marshal::Marshaller;
use crate::marshal::NativeType;
use crate::marshal::TypeID;
use crate::DynValue;
use crate::ObjectID;
use crate::StructValue;
use smallvec::SmallVec;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::rc::Rc;
use terapascal_ir::generic::instantiate_struct_def;
use terapascal_ir::TypeRef;
use terapascal_ir::InstructionBuilder;
use terapascal_ir::MetadataSource;

#[derive(Debug, Clone)]
pub struct StructFieldInfo {
    pub offset: usize,
    pub ty: ir::Type,
    pub native_type: NativeType,
}

#[derive(Debug, Clone)]
pub struct StructLayout {
    pub def: Rc<ir::StructDef>,
    pub size: usize,
    pub fields: BTreeMap<ir::FieldID, StructFieldInfo>,
}

impl Marshaller {
    pub(super) fn marshal_struct(&mut self, struct_val: &StructValue, out_bytes: &mut [u8]) -> MarshalResult<usize> {
        let Some(layout) = self.struct_layouts.get(&struct_val.type_index) else {
            // struct type is not in metadata
            let struct_type = self.get_type(struct_val.type_index)?;
            return Err(MarshalError::InvalidStructType(struct_type.clone()));
        };

        let struct_size = layout.size;

        let mut field_types: SmallVec<[_; 4]> = SmallVec::new();
        for (field_id, field_info) in &layout.fields {
            field_types.push((field_id.0, field_info.offset));
        }

        for (field_index, field_offset) in field_types {
            // skip fields that aren't present in the value
            if let Some(field_val) = struct_val.fields.get(field_index) {
                let field_bytes = &mut out_bytes[field_offset..];
                self.marshal(field_val, field_bytes)?;
            };
        }

        Ok(struct_size)
    }

    pub(super) fn unmarshal_struct(&self, type_index: TypeID, in_bytes: &[u8]) -> MarshalResult<UnmarshalledValue<StructValue>> {
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
            let field_bytes = &in_bytes[field_info.offset..];
            let field_val = self.unmarshal(field_bytes, &field_info.ty)?;
            fields[id.0] = field_val.value;
        }

        Ok(UnmarshalledValue {
            byte_count: layout.size,
            value: StructValue {
                fields,
                type_index,
            },
        })
    }

    pub fn add_struct_type(
        &mut self,
        struct_type: &ir::Type,
    ) -> MarshalResult<(Rc<ir::StructDef>, TypeID)> {
        if let Some(type_index) = self.try_get_type_index(struct_type)
            && let Some(layout) = self.struct_layouts.get(&type_index)
        {
            return Ok((layout.def.clone(), type_index));
        }

        let type_def_id = match struct_type {
            ir::Type::Struct(id) => {
                id.clone()
            },

            ir::Type::Flags(id) => {
                ir::TypeRef::new(*id, [])
            },

            // for object types that are pointers to an inner struct, instantiate that inner
            // struct and register the object type separately
            ir::Type::Object(ir::ObjectID::Class(def_id))
            | ir::Type::WeakObject(ir::ObjectID::Class(def_id)) => {
                let type_index = self.register_type(struct_type.clone(), NativeType::pointer())?;

                let (def, struct_index) = self.add_struct_type(&def_id.to_struct_type())?;
                self.object_id_indices.insert(type_index, ObjectID::Struct(struct_index));

                return Ok((def, type_index));
            }

            _ => {
                return Err(MarshalError::unsupported_type(struct_type.clone()));
            }
        };

        let generic_def = self
            .metadata()
            .get_struct_def(type_def_id.def_id)
            .cloned()
            .ok_or_else(|| {
                MarshalError::MissingTypeDef(struct_type.clone())
            })?;

        let (def, type_index) = match instantiate_struct_def(&generic_def, &type_def_id.args) {
            Cow::Borrowed(..) => {
                // not generic
                let def = Rc::new(generic_def);
                let (_, type_index) = self.define_struct(struct_type, def.clone())?;

                (def, type_index)
            },

            Cow::Owned(new_def) => {
                if self.trace_generics {
                    let generic_name = generic_def.identity.to_pretty_string(self.metadata());
                    let new_name = new_def.identity.to_pretty_string(self.metadata());

                    eprintln!("[vm] new instantiation of struct {}: {}", generic_name, new_name);
                }

                let def = Rc::new(new_def);
                let (_, struct_index) = self.define_struct(struct_type, def.clone())?;

                (def, struct_index)
            },
        };

        if def.identity.is_ref_type() {
            self.gen_class_dtor(&type_def_id)?;
        }

        Ok((def, type_index))
    }

    pub(crate) fn gen_class_dtor(&mut self, type_def_id: &Rc<TypeRef>) -> MarshalResult<()> {
        let class_type = type_def_id.to_class_object_type();

        self.gen_runtime_dtor(&class_type, |builder, self_arg| {
            builder.gen_class_object_dtor_body(type_def_id, self_arg)
        })
    }
}