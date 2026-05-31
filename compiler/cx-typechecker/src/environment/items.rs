use std::collections::HashMap;

use cx_ast::ast::function::CXFunctionKind;
use cx_mir::mir::data::{MIRFunctionPrototype, MIRTemplateInput};
use cx_mir::mir::global::MIRGlobalVariable;
use cx_mir::mir::r#type::MIRType;
use cx_mir::program::MIRFunction;

pub enum MIRFunctionGenRequest {
    Template {
        module_origin: Option<String>,
        kind: CXFunctionKind,
        input: MIRTemplateInput,
    },
    TypeConstructor {
        name: String,
        union_type: MIRType,
        variant_type: MIRType,
        variant_index: usize,
    },
}

pub struct ItemRegistry {
    pub generated_functions: Vec<MIRFunction>,
    pub generated_globals: Vec<MIRGlobalVariable>,
    pub realized_fns: HashMap<String, MIRFunctionPrototype>,
    pub realized_globals: HashMap<String, MIRGlobalVariable>,

    pub requests: Vec<MIRFunctionGenRequest>,

    pub in_external_templated_function: bool,
    pub external_template_origin: Option<String>,
}

impl ItemRegistry {
    pub fn new() -> Self {
        Self {
            generated_functions: Vec::new(),
            generated_globals: Vec::new(),
            realized_fns: HashMap::new(),
            realized_globals: HashMap::new(),

            requests: Vec::new(),
            in_external_templated_function: false,
            external_template_origin: None,
        }
    }

    pub fn get_realized_func(&self, name: &str) -> Option<MIRFunctionPrototype> {
        self.realized_fns.get(name).cloned()
    }
}
