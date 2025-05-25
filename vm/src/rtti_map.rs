use std::collections::BTreeMap;
use std::collections::HashMap;
use std::hash::Hash;
use crate::ir;

#[derive(Debug)]
pub struct RttiMap<Key>
where 
    Key: Copy + Hash + Ord
{
    items: Vec<ir::GlobalRef>,
    items_by_name: HashMap<String, ir::GlobalRef>,
    items_by_key: BTreeMap<Key, ir::GlobalRef>,
}

impl<Key> RttiMap<Key>
where
    Key: Copy + Hash + Ord
{
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            items_by_name: Default::default(),
            items_by_key: BTreeMap::new(),
        }
    }
    
    pub fn add(&mut self, key: Option<Key>, name: Option<String>, value: ir::GlobalRef) {
        if let Some(key) = key {
            self.items_by_key.insert(key, value.clone());
        }

        if let Some(name) = name {
            self.items_by_name.insert(name, value.clone());
        }

        self.items.push(value);
    }
    
    pub fn find_by_name(&self, name: &str) -> Option<&ir::GlobalRef> {
        self.items_by_name.get(name)
    }
    
    pub fn find_by_key(&self, key: Key) -> Option<&ir::GlobalRef> {
        self.items_by_key.get(&key)
    }

    pub fn items(&self) -> &[ir::GlobalRef] {
        &self.items
    }
}
