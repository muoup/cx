use crate::parse::ast::{Expression, FunctionParameter, ValueType};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionPrototype {
    pub(crate) name: String,
    pub(crate) return_type: ValueType,
    pub(crate) args: Vec<FunctionParameter>,
}

pub(crate) struct VerifyContext {
    pub(crate) variable_table: Vec<HashMap<String, ValueType>>,
    pub(crate) function_table: HashMap<String, Rc<FunctionPrototype>>,
    pub(crate) types_table: HashMap<String, ValueType>,

    pub(crate) current_return_type: Option<ValueType>,
}

impl VerifyContext {
    pub(crate) fn push_scope(&mut self) {
        self.variable_table.push(HashMap::new());
    }

    pub(crate) fn pop_scope(&mut self) {
        self.variable_table.pop();
    }

    pub(crate) fn insert_variable(&mut self, name: String, val_type: ValueType) {
        self.variable_table.last_mut().unwrap().insert(name, val_type);
    }

    pub(crate) fn insert_type(&mut self, name: &str, val_type: ValueType) {
        self.types_table.insert(name.to_owned(), val_type);
    }

    pub(crate) fn insert_function(&mut self, name: &str, fn_prototype: FunctionPrototype) -> Option<()> {
        if let Some(func) = self.function_table.get(name) {
            if fn_prototype != **func {
                println!("Function {} already exists with a different prototype", name);
                return None;
            }

            return Some(());
        }

        self.function_table.insert(name.to_owned(), Rc::new(fn_prototype));
        Some(())
    }

    pub(crate) fn get_variable(&self, name: &str) -> Option<&ValueType> {
        for scope in self.variable_table.iter().rev() {
            if let Some(val_type) = scope.get(name) {
                return Some(val_type);
            }
        }

        None
    }

    pub(crate) fn get_function(&self, name: &str) -> Option<Rc<FunctionPrototype>> {
        self.function_table.get(name).cloned()
    }

    pub(crate) fn get_type(&self, name: &str) -> Option<ValueType> {
        self.types_table.get(name).cloned()
    }
}
