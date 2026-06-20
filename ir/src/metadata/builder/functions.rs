use crate::FunctionID;
use crate::FunctionIdentity;
use crate::FunctionInfo;
use crate::FunctionSig;
use crate::MetadataBuilder;
use crate::MetadataCollection;
use crate::NamePath;
use crate::StructDef;
use crate::StructIdentity;
use crate::TagInfo;
use crate::TypeDefID;
use std::rc::Rc;

impl MetadataBuilder {
    pub fn insert_func(
        &mut self,
        identity: FunctionIdentity,
        sig: impl Into<Rc<FunctionSig>>,
        gen_runtime_name: bool,
        tags: impl IntoIterator<Item=TagInfo>,
    ) -> FunctionID {
        let id = self.next_function_id;

        let runtime_name = if gen_runtime_name {
            match &identity {
                FunctionIdentity::Path(path) => {
                    let path_string = path.path.join(".");
                    let id = self.find_or_insert_string(&path_string);

                    Some(id)
                }

                _ => None,
            }
        } else {
            None
        };

        let func_info = FunctionInfo {
            identity,
            runtime_name,

            sig: sig.into(),
            
            tags: tags.into_iter().collect(),

            // up to the frontend to generate and add an invoker later if desired
            invoker: None,
        };

        self.metadata.function_info.insert(id, func_info);
        
        self.next_function_id.0 += 1;

        id
    }
    
    pub fn insert_func_invoker(&mut self, function_id: FunctionID, invoker_id: FunctionID) {
        let Some(function_info) = self.metadata.function_info
            .get_mut(&function_id)
        else {
            panic!("function {} was not declared in this metadata collection", function_id);
        };

        function_info.invoker = Some(invoker_id);
    }
    
    pub fn add_func_tag(&mut self, function_id: FunctionID, tag: TagInfo) {
        let Some(function_info) = self.metadata.function_info
            .get_mut(&function_id)
        else {
            panic!("function {} was not declared in this metadata collection", function_id);
        };

        function_info.tags.push(tag);
    }

    pub fn closures(&self) -> impl Iterator<Item=TypeDefID> {
        self.iter_in_self_or_refs(move |metadata| metadata.closures())
    }

    pub fn closures_by_sig(&self) -> impl Iterator<Item=(Rc<FunctionSig>, &[TypeDefID])> {
        self.iter_in_self_or_refs(move |metadata| metadata.closures_by_sig()
            .iter()
            .map(|(sig, closure_ids)| {
                (sig.clone(), closure_ids.as_slice())
            })
        )
    }
    
    pub fn find_closure_sig(&self, closure_class_id: TypeDefID) -> Option<Rc<FunctionSig>> {
        self.find_in_self_or_refs(move |metadata| {
            metadata.find_closure_sig(closure_class_id)
        })
    }

    pub fn define_closure_ty(&mut self, id: TypeDefID, closure_def: StructDef) {
        let StructIdentity::ClosureObject(identity) = &closure_def.identity else {
            panic!("define_closure_ty: definition struct did not have a closure identity");
        };

        self.metadata.insert_closure(identity.sig.clone(), id);
        self.define_struct(id, closure_def);
    }

    pub fn find_function(&self, name: &NamePath) -> Option<FunctionID> {
        self.find_in_self_or_refs(|metadata| metadata.find_function(name))
    }
}