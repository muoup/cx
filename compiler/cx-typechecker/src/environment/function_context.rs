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

    require_safe: bool,
    require_pure: bool,
    unsafe_depth: usize,

    flow: Option<ControlFlow>,
}

struct LabelRecord {
    declaration: Option<TokenRange>,
    uses: Vec<TokenRange>,
}

#[derive(Clone)]
pub struct FunctionModeSnapshot {
    safe_mode: bool,
    contract_pure_mode: bool,
    unsafe_depth: usize,
}

impl FunctionContext {
    pub fn begin_function(&mut self, prototype: THIRFnPrototype) {
        self.require_safe = prototype.signature().contract.safe;
        self.require_pure = false;
        self.unsafe_depth = 0;
        self.flow = Some(ControlFlow::new());
        self.labels.clear();
        self.current_function = Some(prototype);
    }

    pub fn end_function(&mut self) {
        self.current_function = None;
        self.flow = None;
        self.labels.clear();
        self.require_safe = false;
        self.require_pure = false;
        self.unsafe_depth = 0;
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
            .or_insert_with(|| LabelRecord {
                declaration: None,
                uses: Vec::new(),
            })
            .uses
            .push(range);
    }

    pub fn declare_label(&mut self, name: &CXIdent, range: TokenRange) -> bool {
        let record = self
            .labels
            .entry(name.as_string())
            .or_insert_with(|| LabelRecord {
                declaration: None,
                uses: Vec::new(),
            });
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

    pub fn in_safe_context(&self) -> bool {
        self.require_safe && self.unsafe_depth == 0
    }

    pub fn enter_unsafe(&mut self) {
        self.unsafe_depth += 1;
    }

    pub fn exit_unsafe(&mut self) {
        self.unsafe_depth -= 1;
    }

    pub fn snapshot_mode(&self) -> FunctionModeSnapshot {
        FunctionModeSnapshot {
            safe_mode: self.require_safe,
            contract_pure_mode: self.require_pure,
            unsafe_depth: self.unsafe_depth,
        }
    }

    pub fn set_contract_mode(&mut self, safe: bool) {
        self.require_safe = safe;
        self.require_pure = safe;
        self.unsafe_depth = 0;
    }

    pub fn restore_mode(&mut self, snapshot: FunctionModeSnapshot) {
        self.require_safe = snapshot.safe_mode;
        self.require_pure = snapshot.contract_pure_mode;
        self.unsafe_depth = snapshot.unsafe_depth;
    }
}
