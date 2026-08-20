use cx_tokens::TokenRange;

use crate::{
    MIRBasicBlockID, MIRScopeID,
    global::{MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalVariable},
    ty::registry::MIRTypeRegistryBuilder,
};

#[derive(Debug, Clone)]
pub struct MIRUnit {
    types: MIRTypeRegistryBuilder,
    functions: Vec<MIRFunction>,
    globals: Vec<MIRGlobalVariable>,
}

impl MIRUnit {
    pub fn from_parts(
        types: MIRTypeRegistryBuilder,
        functions: Vec<MIRFunction>,
        globals: Vec<MIRGlobalVariable>,
    ) -> Self {
        Self {
            types,
            functions,
            globals,
        }
    }

    pub fn types(&self) -> &MIRTypeRegistryBuilder {
        &self.types
    }

    pub fn functions(&self) -> &[MIRFunction] {
        &self.functions
    }

    pub fn globals(&self) -> &[MIRGlobalVariable] {
        &self.globals
    }

    pub fn function(&self, id: MIRFunctionID) -> Option<&MIRFunction> {
        self.functions.get(id.index())
    }

    pub fn global(&self, id: MIRGlobalID) -> Option<&MIRGlobalVariable> {
        self.globals.get(id.index())
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
