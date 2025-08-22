use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::value_type::CXType;
use cx_util::scoped_map::ScopedMap;

pub struct TypeEnvironment<'a> {
    ast: &'a CXAST,
    
    symbol_table: ScopedMap<CXType>,
}

impl<'a> TypeEnvironment<'a> {
    pub fn new(ast: &'a CXAST) -> Self {
        Self {
            ast,
            
            symbol_table: ScopedMap::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.symbol_table.push_scope();
    }
    
    pub fn pop_scope(&mut self) {
        self.symbol_table.pop_scope();
    }
    
    pub fn insert_symbol(&mut self, name: String, ty: CXType) {
        self.symbol_table.insert(name, ty);
    }
    
    pub fn get_symbol(&self, name: &str) -> Option<(ValueID, CXType)> {
        Some(
            
        )
    }
}