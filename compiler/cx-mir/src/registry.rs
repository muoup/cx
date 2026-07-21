use std::collections::HashMap;

use cx_target::ArchitectureConfig;

use crate::{
    mir::data::{MIRType, MIRTypeId},
    type_context::MIRTypeContext,
};

//
// After the evaluation and completion of the MIRUnit, this struct contains all necessary context to interpret
// the complete meaning of its contents. For instance, prototypes are not necessary to provide here as a map as
// they are either tacked onto the function definition nodes or in the types applied to the AST nodes, however
// mapping type ids is required as later steps need to be able to interpret type definitions.
//
#[derive(Debug, Clone)]
pub struct MIRDecomposedRegistry {
    architecture: ArchitectureConfig,
    typeid_map: HashMap<MIRTypeId, MIRType>,
}

impl MIRDecomposedRegistry {
    pub fn new(architecture: ArchitectureConfig, typeid_map: HashMap<MIRTypeId, MIRType>) -> Self {
        Self {
            architecture,
            typeid_map,
        }
    }
}

impl MIRTypeContext for MIRDecomposedRegistry {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn resolve_type_id(&self, id: MIRTypeId) -> &MIRType {
        self.typeid_map
            .get(&id)
            .unwrap_or_else(|| panic!("Invalid id {id} in MIRDecomposedRegistry!"))
    }
}
