use crate::{global::{MIRFunction, MIRGlobalVariable}, ty::registry::MIRTypeRegistry};

#[derive(Debug, Clone)]
pub struct MIRUnit {
    types: MIRTypeRegistry,
    functions: Vec<MIRFunction>,
    globals: Vec<MIRGlobalVariable>,
}

impl MIRUnit {
    pub fn new(registry: MIRTypeRegistry, functions: Vec<MIRFunction>, globals: MIRGlobalVariable) -> Self {
        Self {
            types: registry,
            functions,
            globals: vec![globals],
        }
    }

    pub fn types(&self) -> &MIRTypeRegistry {
        &self.types
    }

    pub fn functions(&self) -> &[MIRFunction] {
        &self.functions
    }

    pub fn globals(&self) -> &[MIRGlobalVariable] {
        &self.globals
    }
}
