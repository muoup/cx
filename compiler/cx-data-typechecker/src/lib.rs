pub mod ast;

use cx_data_ast::parse::ast::{CXFunctionPrototype, CXAST};
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use cx_data_ast::parse::type_mapping::contextualize_type;
use cx_data_ast::parse::value_type::CXType;
use cx_data_ast::preparse::pp_type::CXNaiveType;
use cx_util::scoped_map::ScopedMap;

pub struct TCEnvironment<'a> {
    ast: &'a CXAST,

    type_map: &'a CXTypeMap,
    fn_map: &'a CXFunctionMap,

    current_function: Option<CXFunctionPrototype>,
    symbol_table: ScopedMap<CXType>,
}

impl<'a> TCEnvironment<'a> {
    pub fn new(ast: &'a CXAST) -> Self {
        Self {
            ast,

            type_map: &ast.type_map,
            fn_map: &ast.function_map,

            current_function: None,
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

    pub fn symbol_type(&self, name: &str) -> Option<&CXType> {
        self.symbol_table.get(name)
    }

    pub fn get_func(&self, name: &str) -> Option<&CXFunctionPrototype> {
        self.fn_map.get(name)
    }

    pub fn get_type(&self, name: &str) -> Option<&CXType> {
        self.type_map.get(name)
    }

    pub fn current_function(&self) -> &CXFunctionPrototype {
        self.current_function.as_ref()
            .unwrap()
    }

    pub fn contextualize_type(&self, ty: &CXNaiveType) -> Option<CXType> {
        contextualize_type(self.type_map, ty)
    }
}