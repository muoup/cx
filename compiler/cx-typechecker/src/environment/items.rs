use std::collections::HashMap;

use cx_ast::data::CXFunctionKind;
use cx_mir::function_map::CXFnMap;
use cx_mir::mir::data::{MIRFunctionPrototype, MIRTemplateInput};
use cx_mir::mir::program::{MIRFunction, MIRGlobalVariable};

pub enum MIRFunctionGenRequest {
    Template {
        module_origin: Option<String>,
        kind: CXFunctionKind,
        input: MIRTemplateInput,
    },
}

pub struct ItemRegistry {
    pub realized_fns: CXFnMap,
    pub realized_globals: HashMap<String, MIRGlobalVariable>,
    pub generated_functions: Vec<MIRFunction>,
    pub requests: Vec<MIRFunctionGenRequest>,
    pub in_external_templated_function: bool,
    pub external_template_origin: Option<String>,
}

impl ItemRegistry {
    pub fn new() -> Self {
        Self {
            realized_fns: HashMap::new(),
            realized_globals: HashMap::new(),
            generated_functions: Vec::new(),
            requests: Vec::new(),
            in_external_templated_function: false,
            external_template_origin: None,
        }
    }

    pub fn get_realized_func(&self, name: &str) -> Option<MIRFunctionPrototype> {
        self.realized_fns.get(name).cloned()
    }
}
