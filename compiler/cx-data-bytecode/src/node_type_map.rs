use std::collections::{HashMap, HashSet};
use cx_data_ast::parse::ast::CXExpr;
use cx_data_ast::parse::value_type::CXType;

#[derive(Debug)]
pub struct TypeCheckData {
    expr_types: HashMap<u64, CXType>,
    deferring_functions: HashSet<String>,
}

impl Default for TypeCheckData {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeCheckData {
    fn node_hash(&self, expr: &CXExpr) -> u64 {
        // Using the address can't possibly go wrong, right?
        expr.uuid as u64
    }

    pub fn new() -> Self {
        TypeCheckData {
            expr_types: HashMap::new(),
            deferring_functions: HashSet::new(),
        }
    }
    
    pub fn insert(&mut self, expr: &CXExpr, cx_type: CXType) -> Option<&CXType> {
        let node_id = self.node_hash(expr);
        
        self.expr_types.insert(node_id, cx_type);
        self.expr_types.get(&node_id)
    }
    
    pub fn expr_type(&self, expr: &CXExpr) -> Option<&CXType> {
        let node_id = self.node_hash(expr);
        
        self.expr_types.get(&node_id)
    }
    
    pub fn set_deferring_function(&mut self, name: String) {
        self.deferring_functions.insert(name);
    }
    
    pub fn function_defers(&self, name: &str) -> bool {
        self.deferring_functions.contains(name)
    }
}