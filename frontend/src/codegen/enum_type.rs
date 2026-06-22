use crate::ir;
use crate::typ::SYSTEM_UNIT_NAME;

pub const ENUM_MEMBER_TAG_NAME: &str = "PascalEnumMember";
pub const ENUM_MEMBER_TAG_NAME_FIELD: ir::FieldID = ir::FieldID(0);

#[derive(Copy, Clone)]
pub struct EnumMemberTagInfo {
    pub class_id: ir::TypeDefID,

    pub name_field: ir::FieldID,
}

impl EnumMemberTagInfo {
    pub fn find_in_metadata(metadata: &impl ir::MetadataSource) -> Option<Self> {
        let name_path = ir::NamePath::new([SYSTEM_UNIT_NAME.to_string()], ENUM_MEMBER_TAG_NAME);

        let (id, _def) = metadata.find_struct_def(&name_path)?;

        Some(Self {
            class_id: id,
            name_field: ENUM_MEMBER_TAG_NAME_FIELD,
        })
    }
}