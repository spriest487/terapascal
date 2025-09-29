use std::rc::Rc;
use crate::{MetadataBuilder, TagLocation};
use crate::DynArrayRuntimeType;
use crate::RuntimeType;
use crate::Type;

impl MetadataBuilder {
    pub fn insert_runtime_type(&mut self, ty: Type, runtime_type: RuntimeType) -> Rc<RuntimeType> {
        let runtime_type = Rc::new(runtime_type);

        // it's valid to replace existing entries
        // getting the runtime type info right is the responsibility of the frontend 
        self.metadata.runtime_types.insert(ty, runtime_type.clone());

        runtime_type
    }

    pub fn declare_dynarray_runtime_type(&mut self, element_ty: &Type) -> DynArrayRuntimeType {
        if self.dyn_array_runtime_types.contains_key(element_ty) {
            panic!("duplicate rc boilerplate declaration for type {}", self.pretty_ty_name(element_ty));
        }

        let runtime_type = DynArrayRuntimeType {
            alloc: self.insert_func(None),
            length: self.insert_func(None),
        };

        self.metadata.dyn_array_runtime_types.insert(element_ty.clone(), runtime_type.clone());
        runtime_type
    }

    pub fn alloc_tag_array(&mut self, loc: TagLocation, len: usize) {
        self.metadata.tag_counts.insert(loc, len);
    }
}
