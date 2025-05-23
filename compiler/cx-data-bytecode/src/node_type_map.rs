use std::collections::HashMap;
use cx_data_ast::parse::ast::CXExpr;
use cx_data_ast::parse::value_type::CXType;

#[derive(Debug)]
pub struct ExprTypeMap {
    inner: HashMap<u64, CXType>
}

impl ExprTypeMap {
    fn node_hash(&self, expr: &CXExpr) -> u64 {
        // Using the address can't possibly go wrong, right?
        expr.uuid as u64
    }

    pub fn new() -> Self {
        ExprTypeMap {
            inner: HashMap::new()
        }
    }
    
    pub fn insert(&mut self, expr: &CXExpr, cx_type: CXType) -> Option<&CXType> {
        let node_id = self.node_hash(expr);
        
        self.inner.insert(node_id, cx_type);
        self.inner.get(&node_id)
    }
    
    pub fn get(&self, expr: &CXExpr) -> Option<&CXType> {
        let node_id = self.node_hash(expr);
        
        self.inner.get(&node_id)
    }
}