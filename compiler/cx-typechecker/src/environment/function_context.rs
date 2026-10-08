use std::collections::HashMap;

use cx_log::CXRawResult;
use cx_thir::thir::data::THIRFnPrototype;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::environment::control_flow::{ControlFlow, ScopeEffects};

#[derive(Default)]
pub struct FunctionContext {
    current_function: Option<THIRFnPrototype>,
    labels: HashMap<String, LabelRecord>,

    flow: Option<ControlFlow>,
}

#[derive(Default)]
struct LabelRecord {
    declaration: Option<TokenRange>,
    uses: Vec<TokenRange>,
    address_taken: bool,
}

impl FunctionContext {
    pub fn begin_function(&mut self, prototype: THIRFnPrototype) {
        self.flow = Some(ControlFlow::new());
        self.labels.clear();
        self.current_function = Some(prototype);
    }

    pub fn end_function(&mut self) {
        self.current_function = None;
        self.flow = None;
        self.labels.clear();
    }

    pub fn current_function(&self) -> &THIRFnPrototype {
        self.current_function.as_ref().unwrap()
    }

    pub fn try_current_function(&self) -> Option<&THIRFnPrototype> {
        self.current_function.as_ref()
    }

    pub fn record_label_use(&mut self, name: &CXIdent, range: TokenRange) {
        self.labels
            .entry(name.as_string())
            .or_default()
            .uses
            .push(range);
    }

    pub fn record_label_address(&mut self, name: &CXIdent, range: TokenRange) {
        self.record_label_use(name, range);
        self.labels.entry(name.as_string()).or_default().address_taken = true;
    }

    /// The labels whose address is taken, in a stable order.
    pub fn address_taken_labels(&self) -> Vec<CXIdent> {
        let mut labels: Vec<_> = self
            .labels
            .iter()
            .filter(|(_, record)| record.address_taken)
            .map(|(name, _)| CXIdent::new(name.as_str()))
            .collect();
        labels.sort_by(|a, b| a.as_str().cmp(b.as_str()));
        labels
    }

    pub fn declare_label(&mut self, name: &CXIdent, range: TokenRange) -> bool {
        let record = self.labels.entry(name.as_string()).or_default();
        if record.declaration.is_some() {
            return false;
        }
        record.declaration = Some(range);
        true
    }

    pub fn unresolved_label(&self) -> Option<(&str, &TokenRange)> {
        self.labels.iter().find_map(|(name, record)| {
            record
                .declaration
                .is_none()
                .then(|| record.uses.first().map(|range| (name.as_str(), range)))
                .flatten()
        })
    }

    pub fn flow(&self) -> &ControlFlow {
        self.flow
            .as_ref()
            .expect("function control-flow state is only available while checking a function body")
    }

    pub fn flow_mut(&mut self) -> &mut ControlFlow {
        self.flow
            .as_mut()
            .expect("function control-flow state is only available while checking a function body")
    }

    pub fn pop_scope(&mut self) -> CXRawResult<ScopeEffects> {
        self.flow_mut().pop_scope()
    }
}
