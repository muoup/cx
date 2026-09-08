use std::collections::HashMap;

use cx_log::{CXRawResult, catalogue::mir as catalogue};
use cx_tokens::TokenRange;

use crate::{
    MIRBasicBlockID, MIRConstant, MIRScopeID,
    global::{
        MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalKind, MIRGlobalState, MIRGlobalVariable,
    },
    ty::registry::MIRTypeRegistryBuilder,
};

#[derive(Debug, Clone)]
pub struct MIRUnit {
    types: MIRTypeRegistryBuilder,
    functions: HashMap<MIRFunctionID, MIRFunction>,
    globals: HashMap<MIRGlobalID, MIRGlobalVariable>,
    global_order: Vec<MIRGlobalID>,
}

impl MIRUnit {
    pub fn new(
        types: MIRTypeRegistryBuilder,
        functions: HashMap<MIRFunctionID, MIRFunction>,
        globals: HashMap<MIRGlobalID, MIRGlobalVariable>,
        global_order: Vec<MIRGlobalID>,
    ) -> Self {
        Self {
            types,
            functions,
            globals,
            global_order,
        }
    }

    pub fn types(&self) -> &MIRTypeRegistryBuilder {
        &self.types
    }

    pub fn functions(&self) -> impl ExactSizeIterator<Item = &MIRFunction> {
        self.functions.values()
    }

    pub fn globals(&self) -> impl ExactSizeIterator<Item = &MIRGlobalVariable> {
        self.globals.values()
    }

    pub fn global_order(&self) -> &[MIRGlobalID] {
        &self.global_order
    }

    pub fn globals_in_order(&self) -> impl ExactSizeIterator<Item = &MIRGlobalVariable> {
        self.global_order.iter().map(|id| {
            self.global(*id)
                .expect("MIR global order contains an invalid ID")
        })
    }

    pub fn function(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.functions.get(&id)
    }

    pub fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable> {
        self.globals.get(&id)
    }

    pub fn materialize_global(&mut self, id: MIRGlobalID, value: MIRConstant) -> CXRawResult<()> {
        let global = self
            .globals
            .get_mut(&id)
            .ok_or_else(|| crate::log::raw_error(&catalogue::MIR_GLOBAL_MISSING, id.to_string()))?;
        let MIRGlobalKind::Variable { state, .. } = &mut global.kind else {
            return crate::log::log_raw_error(
                &catalogue::MIR_GLOBAL_NOT_VARIABLE,
                id.to_string(),
            );
        };
        if !matches!(&*state, MIRGlobalState::Initializer(_)) {
            return crate::log::log_raw_error(
                &catalogue::MIR_GLOBAL_NOT_INITIALIZER,
                id.to_string(),
            );
        }
        *state = MIRGlobalState::Initialized(value);
        Ok(())
    }

    pub fn instruction_range(
        &self,
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
    ) -> Option<&TokenRange> {
        self.function(function)
            .and_then(|function| function.definition())
            .and_then(|definition| definition.block(block))
            .and_then(|block| block.instrs.get(instruction))
            .map(|instruction| &instruction.token_range)
    }

    pub fn scope_range(&self, function: MIRFunctionID, scope: MIRScopeID) -> Option<&TokenRange> {
        self.function(function)
            .and_then(|function| function.definition())
            .and_then(|definition| definition.scope(scope))
            .map(|scope| &scope.token_range)
    }
}
