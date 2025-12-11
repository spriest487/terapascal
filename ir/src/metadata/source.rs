use crate::FunctionID;
use crate::FunctionInfo;
use crate::InterfaceDef;
use crate::InterfaceID;
use crate::StructDef;
use crate::TagInfo;
use crate::TagLocation;
use crate::TypeDef;
use crate::TypeDefID;
use crate::VariantDef;

pub trait MetadataSource {
    fn get_struct_def(&self, struct_id: TypeDefID) -> Option<&StructDef>;
    fn get_variant_def(&self, struct_id: TypeDefID) -> Option<&VariantDef>;
    fn type_defs(&self) -> impl Iterator<Item=(TypeDefID, &TypeDef)>;

    fn functions(&self) -> impl Iterator<Item=(FunctionID, &FunctionInfo)>;
    fn interfaces(&self) -> impl Iterator<Item=(InterfaceID, &InterfaceDef)>;

    fn all_tags(&self) -> impl Iterator<Item=(TagLocation, &[TagInfo])> {
        let type_tags = self.type_defs()
            .map(|(id, def)| {
                let tags = match def {
                    TypeDef::Struct(struct_def) => struct_def.tags.as_slice(),
                    TypeDef::Variant(struct_def) => struct_def.tags.as_slice(),
                    TypeDef::Function(_alias_sig) => &[],
                };

                (TagLocation::TypeDef(id), tags)
            });
        
        let func_tags = self.functions()
            .map(|(id, func_info)| {
                (TagLocation::Function(id), func_info.tags.as_slice())
            });
        
        let iface_tags = self.interfaces()
            .map(|(id, iface_def)| {
                (TagLocation::Interface(id), iface_def.tags.as_slice())
            });
        
        type_tags
            .chain(func_tags)
            .chain(iface_tags)
    }
}
