use cx_lexer_data::token::Token;
use cx_parsing_data::preparse::naive_types::{CXNaivePrototype, CXNaiveType};
use cx_parsing_data::preparse::{NaiveFnIdent, NaiveFnKind};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_typechecker_data::intrinsic_types::INTRINSIC_TYPES;
use cx_typechecker_data::CXTypeMap;
use cx_typechecker_data::ast::{TCBaseMappings, TCFunctionDef, TCGlobalVariable};
use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_typechecker_data::function_map::{CXFnMap, CXFunctionIdentifier};
use cx_util::scoped_map::ScopedMap;
use std::collections::{HashMap, HashSet};

use crate::type_completion::templates::instantiate_function_template;
use crate::type_completion::{complete_prototype, complete_type};

pub struct TCTemplateRequest {
    pub module_origin: Option<String>,
    pub name: NaiveFnKind,
    pub input: CXTemplateInput,
}

pub struct TCEnvironment<'a> {
    pub tokens: &'a [Token],
    pub compilation_unit: CompilationUnit,

    pub module_data: &'a ModuleData,

    pub base_data: &'a TCBaseMappings,

    pub realized_types: CXTypeMap,
    pub realized_fns: CXFnMap,
    pub realized_globals: HashMap<String, TCGlobalVariable>,

    pub requests: Vec<TCTemplateRequest>,
    pub deconstructors: HashSet<CXType>,

    pub current_function: Option<CXFunctionPrototype>,
    pub symbol_table: ScopedMap<CXType>,

    pub declared_functions: Vec<TCFunctionDef>,
    
    pub in_external_templated_function: bool,
}

impl TCEnvironment<'_> {
    pub fn new<'a>(
        tokens: &'a [Token],
        compilation_unit: CompilationUnit,
        structure_data: &'a TCBaseMappings,
        module_data: &'a ModuleData,
    ) -> TCEnvironment<'a> {
        let intrinsic_types = INTRINSIC_TYPES
            .iter()
            .map(|(name, ty)| (name.to_string(), ty.clone().into()))
            .collect::<HashMap<_, _>>();
        
        TCEnvironment {
            tokens,
            compilation_unit,

            module_data,

            base_data: structure_data,
            realized_types: intrinsic_types,
            realized_fns: HashMap::new(),
            realized_globals: HashMap::new(),

            current_function: None,

            requests: Vec::new(),
            deconstructors: HashSet::new(),
            symbol_table: ScopedMap::new(),
            declared_functions: Vec::new(),
            
            in_external_templated_function: false,
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

    pub fn add_type(&mut self, name: String, ty: CXType) {
        self.realized_types.insert(name, ty);
    }

    pub fn symbol_type(&self, name: &str) -> Option<&CXType> {
        self.symbol_table.get(name)
    }

    pub fn get_func(&mut self, name: &NaiveFnIdent) -> Option<CXFunctionPrototype> {
        let Some(base_fn) = self.base_data.fn_data.get_standard(name) else {
            return None;
        };

        complete_prototype(self, self.base_data, base_fn.external_module.as_ref(), &base_fn.resource)
    }

    pub fn get_func_templated(
        &mut self,
        name: &NaiveFnKind,
        input: &CXTemplateInput,
    ) -> Option<CXFunctionPrototype> {
        instantiate_function_template(self, self.base_data, name, input)
    }

    pub fn get_realized_func(&self, name: &CXFunctionIdentifier) -> Option<CXFunctionPrototype> {
        self.realized_fns.get(name).cloned()
    }

    pub fn get_type(&mut self, name: &str) -> Option<CXType> {
        let Some(_ty) = self.base_data.type_data.get_standard(&name.to_string()) else {
            return None;
        };
        
        complete_type(self, self.base_data, _ty.external_module.as_ref(), &_ty.resource)
    }
    
    pub fn get_realized_type(&self, name: &str) -> Option<CXType> {
        self.realized_types.get(name).cloned()
    }

    pub fn destructor_exists(&self, _type: &CXType) -> bool {
        let Some(type_name) = _type.get_identifier() else {
            return false;
        };

        self.base_data
            .fn_data
            .is_key_any(&NaiveFnIdent::Destructor(type_name.clone()))
    }

    pub fn current_function(&self) -> &CXFunctionPrototype {
        self.current_function.as_ref().unwrap()
    }

    pub fn complete_type(&mut self, _type: &CXNaiveType) -> Option<CXType> {
        complete_type(self, self.base_data, None, _type)
    }

    pub fn complete_prototype(
        &mut self,
        prototype: &CXNaivePrototype,
    ) -> Option<CXFunctionPrototype> {
        complete_prototype(self, self.base_data, None, prototype)
    }

    pub fn complete_fn_ident(
        &mut self,
        ident: &CXFunctionIdentifier,
    ) -> Option<CXFunctionPrototype> {
        if let Some(prototype) = self.get_realized_func(ident) {
            return Some(prototype);
        }

        None
    }

    pub fn extend(&mut self, other: TCEnvironment) {
        self.requests.extend(other.requests);
        self.deconstructors.extend(other.deconstructors);
    }
}
