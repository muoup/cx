use std::collections::HashMap;
use cranelift::codegen::ir;

pub(crate) struct VariableTable {
    data: Vec<HashMap<String, ir::Value>>
}

impl VariableTable {
    pub fn new() -> Self {
        Self {
            data: Vec::new()
        }
    }

    pub fn push_scope(&mut self) {
        self.data.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.data.pop();
    }

    pub fn insert(&mut self, name: String, value: ir::Value) {
        self.data.last_mut().unwrap().insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<&ir::Value> {
        for scope in self.data.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }

        None
    }
}