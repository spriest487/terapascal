use crate::ir;
use crate::typ::SYSTEM_UNIT_NAME;

pub const OUT_PARAM_TAG_NAME: &str = "PascalOutParam";

#[derive(Copy, Clone)]
pub struct OutParamTagInfo {
    pub class_id: ir::TypeDefID,
}

impl OutParamTagInfo {
    pub fn find_in_metadata(metadata: &impl ir::MetadataSource) -> Option<Self> {
        let name_path = ir::NamePath::new([SYSTEM_UNIT_NAME.to_string()], OUT_PARAM_TAG_NAME);

        let (id, _def) = metadata.find_struct_def(&name_path)?;

        Some(Self {
            class_id: id,
        })
    }
}