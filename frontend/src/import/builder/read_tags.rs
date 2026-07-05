use crate::import::builder::ImportBuilder;
use crate::ir;
use crate::typ::Type;
use crate::IntConstant;
use ir::MetadataSource as _;

pub struct ImportedSetType {
    pub item_type: Type,
    pub min: IntConstant,
    pub max: IntConstant,
}

pub struct ImportedEnumMember {
    pub enum_type_id: ir::TypeDefID,
}

impl<'a> ImportBuilder<'a> {
    pub fn read_set_tag(&mut self, tag_info: &ir::TagInfo) -> Option<ImportedSetType> {
        let set_tag_info = self.set_type_tag_info?;

        if tag_info.class_id != set_tag_info.class_id {
            return None;
        }

        let item_type_field_val = tag_info.fields.get(&set_tag_info.item_type_field)?.clone();
        let min_field_val = tag_info.fields.get(&set_tag_info.min_field)?.clone();
        let max_field_val = tag_info.fields.get(&set_tag_info.max_field)?.clone();

        let item_type = match item_type_field_val {
            ir::Value::Ref(ir::Ref::Global(ir::GlobalRef::StaticTypeInfo(item_type))) => {
                // any struct type used as the item type of a set is assumed to be an enum ID
                match item_type.as_ref() {
                    ir::Type::Struct(enum_ref) => {
                        let enum_path = self.get_type_decl(enum_ref.def_id)?.name()?.clone();
                        let enum_def_name = self.read_decl_path(&enum_path).ok()?;

                        Type::enumeration(enum_def_name.full_path)
                    }

                    ty => {
                        self.read_type(ty).ok()?
                    }
                }
            }

            _ => {
                // invalid
                return None
            },
        };

        let min = match min_field_val {
            ir::Value::LiteralI64(min_lit) => IntConstant::from(min_lit as i128),
            _ => return None,
        };

        let max = match max_field_val {
            ir::Value::LiteralI64(min_lit) => IntConstant::from(min_lit as i128),
            _ => return None,
        };

        Some(ImportedSetType {
            item_type,
            min,
            max,
        })
    }

    pub fn read_enum_member_tag(&self, tag_info: &ir::TagInfo) -> Option<ImportedEnumMember> {
        let enum_member_tag_info = self.enum_member_tag_info?;

        if tag_info.class_id != enum_member_tag_info.class_id {
            return None;
        }

        let enum_type_id = match tag_info.fields.get(&enum_member_tag_info.name_field)? {
            ir::Value::Ref(ir::Ref::Global(ir::GlobalRef::StaticTypeInfo(enum_type))) => {
                enum_type.definition_ref()?.def_id
            }
            _ => return None,
        };

        Some(ImportedEnumMember {
            enum_type_id,
        })
    }
}