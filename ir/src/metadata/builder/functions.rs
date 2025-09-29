use std::rc::Rc;
use crate::{FunctionDecl, FunctionID, MetadataBuilder, NamePath, StaticClosureID, Struct, TypeDefID};

impl MetadataBuilder {
    pub fn insert_func(&mut self, global_name: Option<NamePath>) -> FunctionID {
        let id = self.next_function_id;

        let runtime_name = global_name
            .as_ref()
            .cloned()
            .map(|name_path| {
                let name = name_path.to_string();

                self.find_or_insert_string(&name)
            });

        let decl = FunctionDecl { global_name, runtime_name };
        self.metadata.functions.insert(id, Rc::new(decl));
        
        self.next_function_id.0 += 1;

        id
    }

    pub fn insert_dtor(&mut self, owning_type: TypeDefID, dtor_func: FunctionID) {
        self.metadata.dtors.insert(owning_type, dtor_func);
    }

    pub fn insert_static_closure(&mut self, func_id: FunctionID, closure: StaticClosureID) {
        let replaced = self.metadata.function_static_closures.insert(func_id, closure);
        assert!(replaced.is_none(), "static closure for function {func_id} must not have been inserted already");
    }

    pub fn define_closure_ty(&mut self, id: TypeDefID, closure_def: Struct) {
        self.define_struct(id, closure_def);
        self.metadata.closures.push(id);
    }
}
