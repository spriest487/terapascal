use crate::ir;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug)]
pub struct RttiMap<Key>
where 
    Key: Clone + Eq + Hash
{
    items: Vec<ir::GlobalRef>,
    items_by_name: HashMap<String, ir::GlobalRef>,
    items_by_key: HashMap<Key, ir::GlobalRef>,
    names_by_key: HashMap<Key, String>,
}

impl<Key> RttiMap<Key>
where
    Key: Clone + Eq + Hash
{
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            items_by_name: Default::default(),
            items_by_key: Default::default(),
            names_by_key: Default::default(),
        }
    }
    
    pub fn add(&mut self, key: Option<Key>, name: Option<String>, value: ir::GlobalRef) {
        match (key, name) {
            (Some(key), None) => {
                self.items_by_key.insert(key, value.clone());
            }

            (None, Some(name)) => {
                self.items_by_name.insert(name.clone(), value.clone());
            }

            (Some(key), Some(name)) => {
                self.items_by_key.insert(key.clone(), value.clone());
                self.items_by_name.insert(name.clone(), value.clone());
                self.names_by_key.insert(key, name);
            }

            (None, None) => {
            }
        }

        self.items.push(value);
    }
    
    pub fn find_by_name(&self, name: &str) -> Option<&ir::GlobalRef> {
        self.items_by_name.get(name)
    }

    pub fn name_by_key(&self, by_key: &Key) -> Option<&str> {
        self.names_by_key.get(by_key).map(String::as_str)
    }
    
    pub fn find_by_key(&self, key: &Key) -> Option<&ir::GlobalRef> {
        self.items_by_key.get(key)
    }

    pub fn items(&self) -> &[ir::GlobalRef] {
        &self.items
    }
}
