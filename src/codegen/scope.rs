use std::collections::HashMap;
use cranelift::codegen::ir;

pub(crate) struct VariableTable {
    data: Vec<HashMap<String, (ir::Value, ir::Type)>>
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

    pub fn insert(&mut self, name: String, value: ir::Value, type_: ir::Type) {
        self.data.last_mut().unwrap().insert(name, (value, type_));
    }

    pub fn get(&self, name: &str) -> Option<&(ir::Value, ir::Type)> {
        for scope in self.data.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }

        None
    }
}