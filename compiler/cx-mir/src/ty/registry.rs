use std::collections::HashMap;

use cx_target::ArchitectureConfig;

use crate::ty::interface::MTRegistry;

use super::{MIRType, MIRTypeID};

#[derive(Debug, Clone)]
pub struct MIRTypeRegistry {
    architecture: ArchitectureConfig,
    definitions: HashMap<MIRTypeID, MIRType>,
    debug_names: HashMap<MIRTypeID, String>,
}

impl MIRTypeRegistry {
    pub fn new(
        architecture: ArchitectureConfig,
        definitions: HashMap<MIRTypeID, MIRType>,
        debug_names: HashMap<MIRTypeID, String>,
    ) -> Self {
        Self {
            architecture,
            definitions,
            debug_names,
        }
    }
}

impl MTRegistry for MIRTypeRegistry {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn definition(&self, id: MIRTypeID) -> Option<&MIRType> {
        self.definitions.get(&id)
    }

    fn find(&self, ty: &MIRType) -> Option<MIRTypeID> {
        self.definitions
            .iter()
            .position(|t| t.1 == ty)
            .map(MIRTypeID::new)
    }

    fn find_kind(&self, kind: &super::MIRTypeKind) -> Option<MIRTypeID> {
        self.definitions
            .iter()
            .position(|t| &t.1.kind == kind)
            .map(MIRTypeID::new)
    }

    fn debug_name(&self, id: MIRTypeID) -> Option<&str> {
        self.debug_names.get(&id).map(|s| s.as_str())
    }
}
