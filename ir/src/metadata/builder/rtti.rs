use crate::MetadataBuilder;
use crate::TypeInfo;
use crate::TagLocation;
use crate::Type;
use std::rc::Rc;

impl MetadataBuilder {
    pub fn insert_runtime_type(&mut self, ty: Type, runtime_type: TypeInfo) -> Rc<TypeInfo> {
        let runtime_type = Rc::new(runtime_type);

        // it's valid to replace existing entries
        // getting the runtime type info right is the responsibility of the frontend 
        self.metadata.types.insert(ty, runtime_type.clone());

        runtime_type
    }

    pub fn get_runtime_type(&self, ty: &Type) -> Option<Rc<TypeInfo>> {
        self.find_in_self_or_refs(move |metadata| metadata.get_typeinfo(ty))
    }

    pub fn alloc_tag_array(&mut self, loc: TagLocation, len: usize) {
        self.metadata.tag_counts.insert(loc, len);
    }
}
