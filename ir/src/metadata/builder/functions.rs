use crate::FunctionID;
use crate::MetadataBuilder;
use crate::NamePath;
use crate::StaticClosureID;
use crate::StructDef;
use crate::TypeDefID;
use crate::FunctionDecl;
use std::rc::Rc;

impl MetadataBuilder {
    pub fn get_function(&self, id: FunctionID) -> Option<&Rc<FunctionDecl>> {
        self.find_in_self_or_refs(move |metadata| metadata.get_function(id))
    }
    
    pub fn insert_func(&mut self, global_name: Option<NamePath>) -> FunctionID {
        let id = self.next_function_id;

        let runtime_name = global_name
            .as_ref()
            .cloned()
            .map(|name_path| {
                let name = name_path.to_string();

                self.find_or_insert_string(&name)
            });

        let decl = FunctionDecl { global_name: global_name.clone(), runtime_name };
        self.metadata.functions.insert(id, Rc::new(decl));
        
        self.next_function_id.0 += 1;

        id
    }

    pub fn insert_dtor(&mut self, owning_type: TypeDefID, dtor_func: FunctionID) {
        self.metadata.dtors.insert(owning_type, dtor_func);
    }

    pub fn find_dtor(&self, type_id: TypeDefID) -> Option<FunctionID> {
        self.find_in_self_or_refs(|metadata| metadata.find_dtor(type_id))
    }
    
    pub fn closures(&self) -> impl Iterator<Item=TypeDefID> {
        self.iter_in_self_or_refs(move |metadata| metadata.closures().iter().cloned())
    }
    
    pub fn get_static_closure(&self, func_id: FunctionID) -> Option<StaticClosureID> {
        self.find_in_self_or_refs(move |metadata| metadata.get_static_closure(func_id))
    }

    pub fn insert_static_closure(&mut self, func_id: FunctionID, closure: StaticClosureID) {
        let replaced = self.metadata.function_static_closures.insert(func_id, closure);
        assert!(replaced.is_none(), "static closure for function {func_id} must not have been inserted already");
    }

    pub fn define_closure_ty(&mut self, id: TypeDefID, closure_def: StructDef) {
        self.define_struct(id, closure_def);
        self.metadata.closures.push(id);
    }

    pub fn find_function(&self, name: &NamePath) -> Option<FunctionID> {
        self.find_in_self_or_refs(|metadata| metadata.find_function(name))
    }
}
