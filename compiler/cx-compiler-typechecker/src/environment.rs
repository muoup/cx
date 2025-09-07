use crate::templates::{instantiate_function_template, instantiate_type_template};
use cx_data_typechecker::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_data_typechecker::{CXFnData, CXFnMap, CXTypeData, CXTypeMap};
use cx_util::mangling::{mangle_destructor, mangle_template};
use cx_util::scoped_map::ScopedMap;
use std::collections::{HashMap, HashSet};
use cx_data_ast::parse::ast::{CXGlobalVariable, CXAST};
use cx_data_typechecker::ast::{TCFunctionDef, TCGlobalVariable, TCStructureData};

pub struct TCTemplateRequest {
    pub module_origin: Option<String>,
    pub name: String,
    pub input: CXTemplateInput
}

pub struct TCEnvironment<'a> {
    pub base_data: &'a TCStructureData,
    pub realized_types: CXTypeMap,
    pub realized_fns: CXFnMap,

    pub requests: Vec<TCTemplateRequest>,
    pub deconstructors: HashSet<CXType>,

    pub global_variables: HashMap<String, TCGlobalVariable>,
    pub current_function: Option<CXFunctionPrototype>,
    pub symbol_table: ScopedMap<CXType>,

    pub declared_functions: Vec<TCFunctionDef>,
}

impl TCEnvironment<'_> {
    pub fn new(structure_data: &TCStructureData) -> TCEnvironment {
        TCEnvironment {
            base_data: &structure_data,
            realized_types: HashMap::new(),
            realized_fns: HashMap::new(),

            current_function: None,

            global_variables: HashMap::new(),
            requests: Vec::new(),
            deconstructors: HashSet::new(),
            symbol_table: ScopedMap::new(),
            declared_functions: Vec::new()
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

    pub fn get_func(&self, name: &str) -> Option<CXFunctionPrototype> {
        self.realized_fns
            .get(name).cloned()
            .or_else(|| self.base_data.fn_data.get(name).cloned())
    }

    pub fn get_type(&self, name: &str) -> Option<CXType> {
        self.realized_types
            .get(name).cloned()
            .or_else(|| self.base_data.type_data.get(name).cloned())
    }

    pub fn get_templated_func(&mut self, name: &str, input: &CXTemplateInput) -> Option<CXFunctionPrototype> {
        let mangled_name = mangle_template(name, &input.args);

        self.get_func(&mangled_name)
            .or_else(|| instantiate_function_template(self, name, input))
    }

    pub fn get_templated_type(&mut self, name: &str, input: &CXTemplateInput) -> Option<CXType> {
        let mangled_name = mangle_template(name, &input.args);

        self.get_type(&mangled_name)
            .or_else(|| instantiate_type_template(self, name, input))
    }

    pub fn destructor_exists(&self, _type: &CXType) -> bool {
        let Some(type_name) = _type.get_name() else {
            return false;
        };

        let mangled_name = mangle_destructor(type_name);

        self.get_func(&mangled_name).is_some()
    }

    pub fn current_function(&self) -> &CXFunctionPrototype {
        self.current_function.as_ref()
            .unwrap()
    }

    pub fn extend(&mut self, other: TCEnvironment) {
        self.requests.extend(other.requests);
        self.deconstructors.extend(other.deconstructors);
    }
}
