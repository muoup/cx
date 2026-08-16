use std::collections::HashSet;

use cx_thir::thir::data::MIRTemplateInput;
use cx_thir::thir::data::THIRFnPrototype;
use cx_thir::thir::data::THIRFunction;
use cx_thir::thir::global::MIRGlobalVariable;
use cx_thir::thir::r#type::THIRType;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

#[derive(Debug)]
pub enum MIRFunctionGenRequest {
    Template {
        name: QualifiedName,
        prototype: THIRFnPrototype,
        input: MIRTemplateInput,
    },
    TypeConstructor {
        symbol_name: String,
        debug_name: CXIdent,
        union_type: THIRType,
        variant_type: THIRType,
        variant_index: usize,
    },
}

pub struct ItemRegistry {
    generated_functions: Vec<THIRFunction>,
    generated_globals: Vec<MIRGlobalVariable>,
    requests: Vec<MIRFunctionGenRequest>,
    requests_fulfilled: HashSet<String>,
}

impl ItemRegistry {
    pub fn new() -> Self {
        Self {
            generated_functions: Vec::new(),
            generated_globals: Vec::new(),

            requests: Vec::new(),
            requests_fulfilled: HashSet::new(),
        }
    }

    pub fn drain_generated_items(self) -> (Vec<THIRFunction>, Vec<MIRGlobalVariable>) {
        if !self.requests.is_empty() {
            unreachable!(
                "Attempted to drain generated items while there are still pending generation requests. This is a bug."
            )
        }

        (self.generated_functions, self.generated_globals)
    }

    pub fn push_request(&mut self, request: MIRFunctionGenRequest) {
        self.requests.push(request);
    }

    pub fn pop_request(&mut self) -> Option<MIRFunctionGenRequest> {
        self.requests.pop()
    }

    pub fn request_fulfilled(&mut self, request_name: &str) -> bool {
        self.requests_fulfilled.contains(request_name)
    }

    pub fn mark_request_fulfilled(&mut self, request_name: String) {
        self.requests_fulfilled.insert(request_name);
    }

    pub fn push_generated_function(&mut self, function: THIRFunction) {
        self.generated_functions.push(function);
    }

    pub fn push_generated_global(&mut self, global: MIRGlobalVariable) {
        self.generated_globals.push(global);
    }
}
