use std::cmp::max;
use crate::marshal::{MarshalError, VariantCaseDataInfo, VariantLayout};
use crate::marshal::MarshalResult;
use crate::marshal::Marshaller;
use crate::marshal::NativeType;
use crate::marshal::StructFieldInfo;
use crate::marshal::StructLayout;
use crate::marshal::TypeIndex;
use std::collections::BTreeMap;
use std::iter;
use std::rc::Rc;
use terapascal_ir as ir;

impl Marshaller {
    pub(super) fn define_struct(
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

        let mut native_fields: Vec<NativeType> = Vec::with_capacity(def_fields.len());
        let mut field_layout = BTreeMap::new();

        let mut offset = 0;

        // the object header isn't included in the struct definition, but the marshaller will
        // offset the struct data to include them in an object's memory allocation
        let header_offset = if def.identity.is_ref_type() {
            Self::object_header_size()
        } else {
            0
        };

        for (field_id, field_def) in def_fields {
            let (_, native_type) = self.build_marshalled_type(&field_def.ty)?;

            if def.layout != ir::StructLayout::Packed {
                let member_align = self.align_of(&field_def.ty, def.layout)?;
                let member_pad = Self::field_padding(offset + header_offset, member_align);

                for _ in 0..member_pad {
                    native_fields.push(NativeType::u8());
                }
            }

            native_fields.push(native_type.clone());

            let size = native_type.size();

            field_layout.insert(*field_id, StructFieldInfo {
                offset,
                ty: field_def.ty.clone(),
                native_type,
            });

            offset += size;
        }

        self.add_dyn_array_type(struct_type.clone())?;

        let native_type = NativeType::structure(native_fields);
        let type_index = self.register_type(struct_type.clone(), native_type.clone())?;

        self.struct_layouts.insert(type_index, StructLayout {
            fields: field_layout,
            size: native_type.size(),
            def,
        });

        // eprintln!("add_struct: {}={} (size {})", type_index.0, struct_type.to_pretty_string(&self.metadata), native_type.size());

        Ok((native_type, type_index))
    }

    pub(super) fn define_variant(
        &mut self,
        variant_type: &ir::Type,
        def: Rc<ir::VariantDef>,
    ) -> MarshalResult<TypeIndex> {
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

    fn align_of(&self, ty: &ir::Type, layout: ir::StructLayout) -> MarshalResult<usize> {
        let type_index = self.get_type_index(ty)?;

        match layout {
            ir::StructLayout::Packed => {
                Ok(1)
            }

            ir::StructLayout::Default => match ty {
                ir::Type::Struct(..) => {
                    let struct_layout = self.struct_layouts
                        .get(&type_index)
                        .ok_or_else(|| {
                            MarshalError::InvalidStructType(ty.clone())
                        })?;

                    let mut max_member_align = 1;
                    for (_, field_def) in &struct_layout.def.fields {
                        let member_align = self.align_of(&field_def.ty, layout)?;
                        max_member_align = usize::max(max_member_align, member_align);
                    }

                    Ok(max_member_align)
                }

                ir::Type::Variant(..) => {
                    let variant_layout = self.variant_layouts
                        .get(&type_index)
                        .ok_or_else(|| {
                            MarshalError::InvalidStructType(ty.clone())
                        })?;

                    let tag_align = self.align_of(&variant_layout.def.tag_type, layout)?;
                    let mut max_data_align = 1;

                    for case in &variant_layout.def.cases {
                        if let Some(data_type) = &case.ty {
                            let data_align = self.align_of(&data_type, layout)?;
                            max_data_align = usize::max(max_data_align, data_align);
                        }
                    }

                    Ok(usize::max(tag_align, max_data_align))
                }

                _ => {
                    let type_index = self.get_type_index(ty)?;
                    let native_type = self.get_native_type(type_index)?;

                    Ok(native_type.size())
                }
            },
        }
    }

    fn field_padding(offset: usize, align: usize) -> usize {
        if offset > 0 && align > 1 {
            match offset % align {
                0 => 0,
                diff => align - diff,
            }
        } else {
            0
        }
    }
}
