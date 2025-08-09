use cx_data_ast::parse::ast::CXExpr;
use cx_data_ast::parse::maps::{CXTemplateRequest, CXTypeMap};
use cx_data_ast::parse::value_type::CXType;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub enum AllocationType {
    None,
    Single,
    Array
}

#[derive(Clone, Debug)]
pub struct DeconstructionType {
    pub index: usize,
    pub has_deconstructor: bool,
    pub allocation_type: AllocationType
}

#[derive(Clone, Debug)]
pub struct DeconstructorData {
    pub _type: CXType,
    pub rec_deconstructor_calls: Vec<usize>,
    pub free_indices: Vec<usize>,
    
    pub deallocations: Vec<DeconstructionType>
}

#[derive(Debug, Default, Clone)]
pub struct TypeCheckData {
    expr_types: HashMap<u64, CXType>,
    deferring_functions: HashSet<String>,
    
    // destructor -- user defined ~[type_name] function
    // deconstructor -- compiler generated function for language-features (i.e. strong pointers)
    pub deconstructor_data: Vec<DeconstructorData>,
    pub requests: Vec<CXTemplateRequest>,
    
    pub full_type_map: CXTypeMap,
    pub full_fn_map: CXTypeMap
}

impl TypeCheckData {
    fn node_hash(&self, expr: &CXExpr) -> u64 {
        expr.uuid
    }

    pub fn new() -> Self {
        TypeCheckData {
            ..Default::default()
        }
    }
    
    pub fn extend(&mut self, other: Self) {
        self.expr_types.extend(other.expr_types);
        self.deferring_functions.extend(other.deferring_functions);
        self.deconstructor_data.extend(other.deconstructor_data);
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
    
    pub fn expr_type_test(&self, uuid: u64) -> Option<&CXType> {
        self.expr_types.get(&uuid)
    }
    
    pub fn set_deferring_function(&mut self, name: String) {
        self.deferring_functions.insert(name);
    }
    
    pub fn function_defers(&self, name: &str) -> bool {
        self.deferring_functions.contains(name)
    }
}