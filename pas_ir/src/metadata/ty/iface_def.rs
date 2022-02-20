use std::collections::HashMap;
use crate::{FunctionID, MethodID, NamePath, Type};

#[derive(Clone, Debug)]
pub struct Method {
    pub name: String,
    pub return_ty: Type,
    pub params: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct Interface {
    pub name: NamePath,
    pub methods: Vec<Method>,
    pub impls: HashMap<Type, InterfaceImpl>,
}

impl Interface {
    pub fn new(name: impl Into<NamePath>, methods: impl Into<Vec<Method>>) -> Self {
        Self {
            name: name.into(),
            methods: methods.into(),
            impls: HashMap::new(),
        }
    }

    pub fn add_impl(&mut self, implementor: Type, method: MethodID, func_id: FunctionID) {
        assert!(method.0 < self.methods.len());

        let methods_len = self.methods.len();
        let impl_entry = self
            .impls
            .entry(implementor.clone())
            .or_insert_with(|| InterfaceImpl::new(methods_len));
        assert!(
            !impl_entry.methods.contains_key(&method),
            "adding duplicate impl (func {}) of method {}.{} for {}, already defined as {}",
            func_id,
            self.name,
            method.0,
            implementor,
            impl_entry.methods[&method],
        );

        impl_entry.methods.insert(method, func_id);
    }

    pub fn method_index(&self, name: &str) -> Option<MethodID> {
        self.methods
            .iter()
            .position(|m| m.name.as_str() == name)
            .map(MethodID)
    }

    pub fn get_method(&self, id: MethodID) -> Option<&Method> {
        self.methods.get(id.0)
    }
}

#[derive(Clone, Debug)]
pub struct InterfaceImpl {
    // method index -> method impl
    pub methods: HashMap<MethodID, FunctionID>,
}

impl InterfaceImpl {
    fn new(method_count: usize) -> Self {
        Self {
            methods: HashMap::with_capacity(method_count),
        }
    }
}