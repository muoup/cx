use std::collections::HashMap;
use std::rc::Rc;
use crate::parse::ast::{VarInitialization, ValueType};
use crate::util::ScopedMap;

pub(crate) type TypeMap = HashMap<String, ValueType>;
pub(crate) type FnMap = HashMap<String, FunctionPrototype>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionPrototype {
    pub(crate) name: String,
    pub(crate) return_type: ValueType,
    pub(crate) args: Vec<VarInitialization>
}

pub(crate) struct VerifyContext {
    pub(crate) type_map: TypeMap,
    pub(crate) fn_map: FnMap,
    pub(crate) variable_table: ScopedMap<ValueType>,

    pub(crate) current_return_type: Option<ValueType>,
    pub(crate) merge_stack: Vec<ValueType>,
}

impl VerifyContext {
    pub(crate) fn push_scope(&mut self) {
        self.variable_table.push_scope()
    }

    pub(crate) fn pop_scope(&mut self) {
        self.variable_table.pop_scope()
    }

    pub(crate) fn insert_variable(&mut self, name: String, val_type: ValueType) {
        self.variable_table.insert(name, val_type);
    }

    pub(crate) fn get_variable(&self, name: &str) -> Option<ValueType> {
        self.variable_table.get(name).cloned()
    }

    pub(crate) fn get_function(&self, name: &str) -> Option<&FunctionPrototype> {
        self.fn_map.get(name)
    }

    pub(crate) fn get_type(&self, name: &str) -> Option<&ValueType> {
        self.type_map.get(name)
    }
}
