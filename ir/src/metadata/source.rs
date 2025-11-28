use crate::StructDef;
use crate::TypeDefID;
use crate::VariantDef;

pub trait MetadataSource {
    fn get_struct_def(&self, struct_id: TypeDefID) -> Option<&StructDef>;
    fn get_variant_def(&self, struct_id: TypeDefID) -> Option<&VariantDef>;
}