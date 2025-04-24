use std::collections::HashMap;
use crate::parse::value_type::ValueType;

pub struct SymbolTable {
    data: HashMap<String, ValueType>,
    scope_rewrites: Vec<Vec<(String, ValueType)>>
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            data: HashMap::new(),
            scope_rewrites: vec![Vec::new()]
        }
    }

    pub fn insert(&mut self, name: String, value: ValueType) {
        if !self.data.contains_key(&name) {
            self.data.insert(name, value);
            return;
        }

        let old_value = self.data.insert(name.clone(), value).unwrap();

        self.scope_rewrites
            .last_mut()
            .unwrap()
            .push((name, old_value));
    }

    pub fn get_symbol(&self, name: &str) -> Option<&ValueType> {
        self.data.get(name)
    }

    pub fn push_scope(&mut self) {
        self.scope_rewrites.push(Vec::new());
    }

    pub fn pop_scope(&mut self) {
        let scope = self.scope_rewrites.pop().unwrap();

        for (name, value) in scope {
            self.data.insert(name, value);
        }
    }
}