use std::collections::HashMap;
use cranelift::codegen::ir;

pub(crate) struct VariableTable {
    data: HashMap<String, ir::Value>,
    overwrites: Vec<Vec<(String, ir::Value)>>
}

impl VariableTable {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
            overwrites: Vec::new()
        }
    }

    pub fn push_scope(&mut self) {
        self.overwrites.push(Vec::new());
    }

    pub fn pop_scope(&mut self) {
        for (name, value) in self.overwrites.pop().unwrap() {
            self.data.insert(name, value);
        }

        self.overwrites.pop();
    }

    pub fn insert(&mut self, name: String, value: ir::Value) {
        if let Some((name, old_value)) = self.data.get_key_value(&name) {
            self.overwrites
                .last_mut().unwrap()
                .push((name.clone(), old_value.clone()));
        }

        self.data.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&ir::Value> {
        self.data.get(name)
    }
}