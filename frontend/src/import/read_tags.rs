use terapascal_ir::MetadataSource;
use crate::import::builder::ImportBuilder;
use crate::ir;
use crate::typ::Type;
use crate::IntConstant;

pub struct ImportedSetType {
    pub item_type: Type,
    pub min: IntConstant,
    pub max: IntConstant,
}

pub struct ImportedEnumMember {
    pub enum_name: String,
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
                self.read_type(&item_type).ok()?
            }
            _ => return None, // invalid
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

        let enum_name = match tag_info.fields.get(&enum_member_tag_info.name_field)? {
            ir::Value::Ref(ir::Ref::Global(ir::GlobalRef::StringLiteral(name_id))) => {
                self.metadata().get_string(*name_id)?.clone()
            }
            _ => return None,
        };

        Some(ImportedEnumMember {
            enum_name,
        })
    }
}