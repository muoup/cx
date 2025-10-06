use cx_lexer_data::token::Token;
use cx_typechecker_data::ast::{TCBaseMappings, TCFunctionDef, TCGlobalVariable};
use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_typechecker_data::function_map::{CXFnMap, CXFunctionKind};
use cx_typechecker_data::CXTypeMap;
use cx_util::scoped_map::ScopedMap;
use std::collections::{HashMap, HashSet};
use std::path::Path;

use crate::type_completion::templates::{instantiate_function_template, instantiate_type_template};

pub struct TCTemplateRequest {
    pub module_origin: Option<String>,
    pub name: CXFunctionKind,
    pub input: CXTemplateInput,
}

pub struct TCEnvironment<'a> {
    pub tokens: &'a [Token],
    pub current_file: &'a Path,

    pub base_data: &'a TCBaseMappings,

    pub realized_types: CXTypeMap,
    pub realized_fns: CXFnMap,
    pub realized_globals: HashMap<String, TCGlobalVariable>,

    pub requests: Vec<TCTemplateRequest>,
    pub deconstructors: HashSet<CXType>,

    pub current_function: Option<CXFunctionPrototype>,
    pub symbol_table: ScopedMap<CXType>,

    pub declared_functions: Vec<TCFunctionDef>,
}

impl TCEnvironment<'_> {
    pub fn new<'a>(
        tokens: &'a [Token],
        file_path: &'a Path,
        structure_data: &'a TCBaseMappings,
    ) -> TCEnvironment<'a> {
        TCEnvironment {
            tokens,
            current_file: file_path,

            base_data: structure_data,
            realized_types: HashMap::new(),
            realized_fns: HashMap::new(),
            realized_globals: HashMap::new(),

            current_function: None,

            requests: Vec::new(),
            deconstructors: HashSet::new(),
            symbol_table: ScopedMap::new(),
            declared_functions: Vec::new(),
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
    
    pub fn func_exists(&self, name: &CXFunctionKind) -> bool {
        self.realized_fns.contains_key(name) || self.base_data.fn_map.contains_generated(name)
    }

    pub fn get_func(&self, name: &CXFunctionKind) -> Option<CXFunctionPrototype> {
        self.realized_fns
            .get(name)
            .cloned()
            .or_else(|| self.base_data.fn_map.get(name).cloned())
    }

    pub fn get_type(&self, name: &str) -> Option<CXType> {
        self.realized_types
            .get(name)
            .cloned()
            .or_else(|| self.base_data.type_data.get(name).cloned())
    }

    pub fn get_global_var(&self, name: &str) -> Option<&TCGlobalVariable> {
        self.realized_globals
            .get(name)
            .or_else(|| self.base_data.global_variables.get(name))
    }

    pub fn get_templated_func(
        &mut self,
        name: &CXFunctionKind,
        input: &CXTemplateInput,
    ) -> Option<CXFunctionPrototype> {
        instantiate_function_template(self, name, input)
    }

    pub fn get_templated_type(&mut self, name: &str, input: &CXTemplateInput) -> Option<CXType> {
        instantiate_type_template(self, name, input)
    }

    pub fn destructor_exists(&self, _type: &CXType) -> bool {
        let Some(type_name) = _type.get_identifier() else {
            return false;
        };
        
        self.get_func(&CXFunctionKind::Destructor { base_type: type_name.clone() }).is_some()
    }

    pub fn current_function(&self) -> &CXFunctionPrototype {
        self.current_function.as_ref().unwrap()
    }

    pub fn extend(&mut self, other: TCEnvironment) {
        self.requests.extend(other.requests);
        self.deconstructors.extend(other.deconstructors);
    }
}
