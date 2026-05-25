use crate::ir;
use crate::marshal::util::UnmarshalledValue;
use crate::marshal::MarshalError;
use crate::marshal::MarshalResult;
use crate::marshal::Marshaller;
use crate::marshal::NativeType;
use crate::marshal::TypeID;
use crate::DynValue;
use crate::Pointer;
use crate::VariantValue;
use std::borrow::Cow;
use std::cmp::max;
use std::iter;
use std::rc::Rc;
use terapascal_ir::generic::instantiate_variant_def;
use terapascal_ir::MetadataSource;

#[derive(Debug, Clone)]
pub struct VariantLayout {
    pub def: Rc<ir::VariantDef>,
    pub size: usize,
    pub data_offset: usize,
    pub cases: Vec<Option<VariantCaseDataInfo>>,
}

#[derive(Debug, Clone)]
pub struct VariantCaseDataInfo {
    pub ty: ir::Type,
    pub native_type: NativeType,
}

impl Marshaller {
    pub(super) fn marshal_variant(
        &mut self,
        variant_val: &VariantValue,
        out_bytes: &mut [u8],
    ) -> MarshalResult<usize> {
        let layout = self.get_variant_layout(variant_val.type_index)?;

        let Some(case_index) = variant_val.tag
            .try_cast(&ir::Type::USize, self)
            .and_then(|usize_val| usize_val.as_usize())
        else {
            return Err(self.invalid_variant_tag_err((*variant_val.tag).clone(), variant_val.type_index));
        };

        let data_native_type = layout.cases
            .get(case_index)
            .map(|case| {
                case.as_ref().map(|data| data.native_type.clone())
            })
            .ok_or_else(|| MarshalError::InvalidData)?;

        let data_offset = layout.data_offset;

        // we still need to refer to the type size, because we always marshal/unmarshal
        // the entire size of the variant regardless of which case is active
        let variant_size = layout.size;

        let tag_size = self.marshal(&variant_val.tag, out_bytes)?;
        if tag_size > data_offset {
            return Err(self.invalid_variant_tag_err((*variant_val.tag).clone(), variant_val.type_index));
        }

        // don't marshal anything for data-less cases
        let data_size = match data_native_type {
            None => 0,
            Some(native_type) => {
                let data_size = self.marshal(&variant_val.data, &mut out_bytes[data_offset..])?;
                let ty_size = native_type.size();
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
            self.get_type(variant_val.type_index)?,
        );

        Ok(variant_size)
    }

    pub(super) fn unmarshal_variant(
        &self,
        type_index: TypeID,
        in_bytes: &[u8],
    ) -> MarshalResult<UnmarshalledValue<VariantValue>> {
        let layout = self.get_variant_layout(type_index)?;

        let tag_val = self.unmarshal(in_bytes, &layout.def.tag_type)?;
        if tag_val.byte_count > layout.data_offset {
            return Err(self.invalid_variant_tag_err(tag_val.value, type_index));
        }

        let Some(case_index) = tag_val.value
            .try_cast(&ir::Type::USize, self)
            .and_then(|usize_val| usize_val.as_usize())
        else {
            return Err(self.invalid_variant_tag_err(tag_val.value, type_index));
        };

        let case_data = layout.cases
            .get(case_index)
            .ok_or_else(|| self.invalid_variant_tag_err(tag_val.value.clone(), type_index))?
            .as_ref();

        let data_val = match case_data {
            Some(case_data) => {
                self.unmarshal(&in_bytes[layout.data_offset..], &case_data.ty)?
            }

            None => {
                UnmarshalledValue {
                    value: DynValue::Pointer(Pointer::nil(ir::Type::Nothing)),
                    byte_count: 0,
                }
            }
        };

        assert!(tag_val.byte_count + data_val.byte_count <= layout.size);

        Ok(UnmarshalledValue {
            byte_count: layout.size,
            value: VariantValue {
                type_index,
                tag: Box::new(tag_val.value),
                data: Box::new(data_val.value),
            },
        })
    }

    fn get_variant_layout(&self, type_index: TypeID) -> MarshalResult<&VariantLayout> {
        match self.variant_layouts.get(&type_index) {
            Some(layout) => Ok(layout),
            None => {
                let variant_type = self.get_type(type_index)?.clone();
                Err(MarshalError::InvalidVariantType(variant_type))
            }
        }
    }

    fn invalid_variant_tag_err(&self, tag_val: DynValue, type_index: TypeID) -> MarshalError {
        let variant_type = match self.get_type(type_index) {
            Ok(t) => t.clone(),
            Err(err) => return err,
        };

        MarshalError::VariantTagOutOfRange {
            variant_type,
            tag: tag_val,
        }
    }

    pub fn variant_data_offset(&self, type_index: TypeID) -> MarshalResult<usize> {
        let layout = self.get_variant_layout(type_index)?;
        Ok(layout.data_offset)
    }

    pub fn add_variant_type(
        &mut self,
        variant_type: &ir::Type
    ) -> MarshalResult<(Rc<ir::VariantDef>, TypeID)> {
        if let Some((_, type_index)) = self.get_cached_type(&variant_type)
            && let Some(layout) = self.variant_layouts.get(&type_index)
        {
            return Ok((layout.def.clone(), type_index));
        }

        let ir::Type::Variant(id) = variant_type else {
            return Err(MarshalError::unsupported_type(variant_type.clone()));
        };

        let generic_def = self.metadata
            .get_variant_def(id.def_id)
            .ok_or_else(|| {
                MarshalError::MissingTypeDef(variant_type.clone())
            })?;

        match instantiate_variant_def(&generic_def, &id.args) {
            Cow::Borrowed(def) => {
                let def = Rc::new(def.clone());
                let type_index = self.define_variant(variant_type, def.clone())?;

                Ok((def, type_index))
            }

            Cow::Owned(new_def) => {
                if self.trace_generics {
                    let generic_name = generic_def.name.to_pretty_string(self.metadata());
                    let new_name = new_def.name.to_pretty_string(self.metadata());

                    eprintln!("[vm] new instantiation of variant {}: {}", generic_name, new_name);
                }

                let def = Rc::new(new_def);
                let type_index = self.define_variant(variant_type, def.clone())?;

                Ok((def, type_index))
            }
        }
    }

    pub(super) fn define_variant(
        &mut self,
        variant_type: &ir::Type,
        def: Rc<ir::VariantDef>,
    ) -> MarshalResult<TypeID> {
        let mut cases = Vec::with_capacity(def.cases.len());

        let mut native_fields = Vec::with_capacity(def.cases.len() + 1);

        let (_, tag_native_type) = self.build_marshalled_type(&def.tag_type)?;

        let tag_offset = tag_native_type.size();

        native_fields.push(tag_native_type);

        let mut max_case_size = 0;
        let mut max_case_pad = 0;

        for case_def in &def.cases {
            if let Some(data_type) = case_def.ty.as_ref() {
                let (_, case_native_type) = self.build_marshalled_type(data_type)?;

                let case_align = self.align_of(&data_type, ir::StructLayout::Default)?;
                let case_pad = Self::field_padding(tag_offset, case_align);

                max_case_pad = max(case_pad, max_case_pad);
                max_case_size = max(case_native_type.size(), max_case_size);

                cases.push(Some(VariantCaseDataInfo {
                    ty: data_type.clone(),
                    native_type: case_native_type,
                }));
            } else {
                cases.push(None)
            };
        }

        let data_offset = tag_offset + max_case_pad;

        // the data value will be addressed by offset only so we just need to fill the appropriate
        // amount of space for all cases
        native_fields.extend(iter::repeat(NativeType::u8()).take(max_case_size + max_case_pad));

        let native_type = NativeType::structure(native_fields);
        let size = native_type.size();

        self.add_dyn_array_type(variant_type.clone())?;
        let type_index = self.register_type(variant_type.clone(), native_type)?;

        self.variant_layouts.insert(type_index, VariantLayout {
            def: def.clone(),
            size,
            data_offset,
            cases,
        });

        Ok(type_index)
    }
}