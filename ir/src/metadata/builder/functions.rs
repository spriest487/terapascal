use crate::FunctionDecl;
use crate::FunctionID;
use crate::MetadataBuilder;
use crate::NamePath;
use crate::StaticClosureID;
use crate::StructDef;
use crate::StructIdentity;
use crate::TypeDefID;
use std::rc::Rc;

impl MetadataBuilder {
    pub fn get_function(&self, id: FunctionID) -> Option<&Rc<FunctionDecl>> {
        self.find_in_self_or_refs(move |metadata| metadata.get_function(id))
    }
    
    pub fn insert_func(&mut self, global_name: Option<NamePath>, gen_runtime_name: bool) -> FunctionID {
        let id = self.next_function_id;

        let runtime_name = if gen_runtime_name {
            global_name
                .as_ref()
                .cloned()
                .map(|name_path| {
                    let name = name_path.to_string();

                    self.find_or_insert_string(&name)
                })
        } else {
            None
        };

        let decl = FunctionDecl { global_name: global_name.clone(), runtime_name };
        self.metadata.functions.insert(id, Rc::new(decl));
        
        self.next_function_id.0 += 1;

        id
    }

    pub fn closures(&self) -> impl Iterator<Item=TypeDefID> {
        self.iter_in_self_or_refs(move |metadata| metadata.closures())
    }

    pub fn closures_by_function(&self) -> impl Iterator<Item=(TypeDefID, &[TypeDefID])> {
        self.iter_in_self_or_refs(move |metadata| metadata.closures_by_function()
            .iter()
            .map(|(func_type_id, closure_ids)| {
                (*func_type_id, closure_ids.as_slice())
            })
        )
    }
    
    pub fn find_closure_func_type_id(&self, closure_class_id: TypeDefID) -> Option<TypeDefID> {
        self.find_in_self_or_refs(move |metadata| {
            metadata.find_closure_func_type_id(closure_class_id)
        })
    }
    
    pub fn get_static_closure(&self, func_id: FunctionID) -> Option<StaticClosureID> {
        self.find_in_self_or_refs(move |metadata| metadata.get_static_closure(func_id))
    }

    pub fn insert_static_closure(&mut self, func_id: FunctionID, closure: StaticClosureID) {
        let replaced = self.metadata.function_static_closures.insert(func_id, closure);
        assert!(replaced.is_none(), "static closure for function {func_id} must not have been inserted already");
    }

    pub fn define_closure_ty(&mut self, id: TypeDefID, closure_def: StructDef) {
        let StructIdentity::Closure(identity) = &closure_def.identity else {
            panic!("define_closure_ty: definition struct did not have a closure identity");
        };

        self.metadata.insert_closure(identity.virt_func_ty, id);
        self.define_struct(id, closure_def);
    }

    pub fn find_function(&self, name: &NamePath) -> Option<FunctionID> {
        self.find_in_self_or_refs(|metadata| metadata.find_function(name))
    }
}
