use crate::templates::instantiate_function_template;
use crate::variable_destruction::visit_destructable_instance;
use cx_data_typechecker::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_data_typechecker::{CXFnData, CXTypeData};
use cx_util::mangling::{mangle_destructor, mangle_template};
use cx_util::scoped_map::ScopedMap;
use std::collections::HashSet;

pub struct TCTemplateRequest {
    pub module_origin: Option<String>,
    pub name: String,
    pub input: CXTemplateInput
}

pub struct TCEnvironment {
    pub type_data: CXTypeData,
    pub fn_data: CXFnData,

    pub requests: Vec<TCTemplateRequest>,
    pub deconstructors: HashSet<CXType>,

    pub current_function: Option<CXFunctionPrototype>,
    pub symbol_table: ScopedMap<CXType>,
}

impl TCEnvironment {
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
        self.fn_data.standard.get(name)
    }

    pub fn get_templated_func(&mut self, name: &str, input: &CXTemplateInput) -> Option<&CXFunctionPrototype> {
        let mangled_name = mangle_template(name, &input.args);

        if !self.fn_data.standard.contains_key(&mangled_name) {
            instantiate_function_template(self, name, input)?;
        }

        self.get_func(&mangled_name)
    }

    pub fn destructor_exists(&self, type_name: &str) -> bool {
        self.fn_data.standard.contains_key(&mangle_destructor(type_name))
    }

    pub fn get_type(&self, name: &str) -> Option<&CXType> {
        self.type_data.standard.get(name)
    }

    pub fn current_function(&self) -> &CXFunctionPrototype {
        self.current_function.as_ref()
            .unwrap()
    }
}
