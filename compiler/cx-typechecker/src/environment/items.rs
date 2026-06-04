use cx_ast::ast::function::CXFunctionKind;
use cx_mir::mir::data::MIRFunction;
use cx_mir::mir::data::MIRTemplateInput;
use cx_mir::mir::global::MIRGlobalVariable;
use cx_mir::mir::r#type::MIRType;

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
    generated_functions: Vec<MIRFunction>,
    generated_globals: Vec<MIRGlobalVariable>,
    requests: Vec<MIRFunctionGenRequest>,
}

impl ItemRegistry {
    pub fn new() -> Self {
        Self {
            generated_functions: Vec::new(),
            generated_globals: Vec::new(),

            requests: Vec::new(),
        }
    }

    pub fn drain_generated_items(self) -> (Vec<MIRFunction>, Vec<MIRGlobalVariable>) {
        if self.requests.len() > 0 {
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

    pub fn push_generated_function(&mut self, function: MIRFunction) {
        self.generated_functions.push(function);
    }

    pub fn push_generated_global(&mut self, global: MIRGlobalVariable) {
        self.generated_globals.push(global);
    }
}
