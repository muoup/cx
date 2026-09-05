use std::collections::HashSet;

use cx_thir::thir::comptime::THIRComptimeFn;
use cx_thir::thir::data::THIRComptimeFnPrototype;
use cx_thir::thir::data::THIRFnPrototype;
use cx_thir::thir::data::THIRFunction;
use cx_thir::thir::data::THIRTemplateInput;
use cx_thir::thir::global::THIRGlobalVariable;
use cx_thir::thir::r#type::THIRType;
use cx_util::{identifier::CXIdent, linkage::LinkageMode, module::QualifiedName};

#[derive(Debug)]
pub enum THIRFunctionGenRequest {
    Template {
        name: QualifiedName,
        prototype: THIRFnPrototype,
        input: THIRTemplateInput,
    },
    Comptime {
        name: QualifiedName,
        prototype: THIRComptimeFnPrototype,
        input: THIRTemplateInput,
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
    generated_comptime_functions: Vec<THIRComptimeFn>,
    generated_globals: Vec<THIRGlobalVariable>,
    requests: Vec<THIRFunctionGenRequest>,
    requests_fulfilled: HashSet<String>,
}

impl ItemRegistry {
    pub fn new() -> Self {
        Self {
            generated_functions: Vec::new(),
            generated_comptime_functions: Vec::new(),
            generated_globals: Vec::new(),

            requests: Vec::new(),
            requests_fulfilled: HashSet::new(),
        }
    }

    pub fn drain_generated_items(
        self,
    ) -> (
        Vec<THIRFunction>,
        Vec<THIRComptimeFn>,
        Vec<THIRGlobalVariable>,
    ) {
        if !self.requests.is_empty() {
            unreachable!(
                "Attempted to drain generated items while there are still pending generation requests. This is a bug."
            )
        }

        (
            self.generated_functions,
            self.generated_comptime_functions,
            self.generated_globals,
        )
    }

    pub fn push_request(&mut self, request: THIRFunctionGenRequest) {
        self.requests.push(request);
    }

    pub fn pop_request(&mut self) -> Option<THIRFunctionGenRequest> {
        self.requests.pop()
    }

    pub fn request_fulfilled(&mut self, request_name: &str) -> bool {
        self.requests_fulfilled.contains(request_name)
    }

    pub fn mark_request_fulfilled(&mut self, request_name: String) {
        self.requests_fulfilled.insert(request_name);
    }

    pub fn push_generated_function(&mut self, function: THIRFunction) {
        if let Some(existing) = self
            .generated_functions
            .iter_mut()
            .find(|existing| existing.prototype.symbol_name() == function.prototype.symbol_name())
        {
            if existing.body.is_none() {
                existing.prototype = function.prototype;
                existing.body = function.body;
            }
            return;
        }

        self.generated_functions.push(function);
    }

    pub fn push_generated_comptime_function(&mut self, function: THIRComptimeFn) {
        self.generated_comptime_functions.push(function);
    }

    pub fn push_generated_global(&mut self, global: THIRGlobalVariable, replace_external: bool) {
        if replace_external
            && global.linkage != LinkageMode::Extern
            && let Some(existing) = self.generated_globals.iter_mut().find(|existing| {
                existing.name == global.name && existing.linkage == LinkageMode::Extern
            })
        {
            *existing = global;
            return;
        }
        self.generated_globals.push(global);
    }

    pub fn generated_global(&self, name: &str) -> Option<&THIRGlobalVariable> {
        self.generated_globals
            .iter()
            .find(|global| global.name.as_str() == name)
    }
}
