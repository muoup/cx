use crate::parse::ast::Expression;
use crate::parse::verify::ValueTypeRef;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(crate) struct FunctionPrototype {
    pub(crate) return_type: ValueTypeRef,
    pub(crate) args: Vec<ValueTypeRef>,
}

pub(crate) struct VerifyContext {
    pub(crate) variable_table: Vec<HashMap<String, ValueTypeRef>>,
    pub(crate) function_table: HashMap<String, FunctionPrototype>,
    pub(crate) types_table: HashMap<String, ValueTypeRef>,

    pub(crate) current_return_type: Option<Expression>,
}

impl VerifyContext {
    pub(crate) fn push_scope(&mut self) {
        self.variable_table.push(HashMap::new());
    }

    pub(crate) fn pop_scope(&mut self) {
        self.variable_table.pop();
    }

    pub(crate) fn insert_variable(&mut self, name: String, val_type: ValueTypeRef) {
        self.variable_table.last_mut().unwrap().insert(name, val_type);
    }

    pub(crate) fn insert_function(&mut self, name: &str, fn_prototype: FunctionPrototype) {
        self.function_table.insert(name.to_owned(), fn_prototype);
    }

    pub(crate) fn get_variable(&self, name: &str) -> Option<&ValueTypeRef> {
        for scope in self.variable_table.iter().rev() {
            if let Some(val_type) = scope.get(name) {
                return Some(val_type);
            }
        }

        None
    }

    pub(crate) fn get_function(&self, name: &str) -> Option<&FunctionPrototype> {
        self.function_table.get(name)
    }

    pub(crate) fn get_type(&self, name: &str) -> ValueTypeRef {
        self.types_table.get(name).unwrap().clone()
    }
}
