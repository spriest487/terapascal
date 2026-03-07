use crate::MetadataBuilder;
use crate::MetadataSource;
use crate::Type;
use crate::TypeInfo;
use std::rc::Rc;

impl MetadataBuilder {
    pub fn insert_type_info(&mut self, ty: Type, type_info: TypeInfo) -> Rc<TypeInfo> {
        let type_info = Rc::new(type_info);

        // it's valid to replace existing entries
        // getting the runtime type info right is the responsibility of the frontend 
        self.metadata.type_info.insert(ty, type_info.clone());

        type_info
    }

    pub fn get_type_info(&self, ty: &Type) -> Option<Rc<TypeInfo>> {
        self.find_in_self_or_refs(move |metadata| metadata.get_type_info(ty))
    }
}
