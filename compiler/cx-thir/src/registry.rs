use std::collections::HashMap;

use cx_target::ArchitectureConfig;

use crate::{
    thir::data::{THIRType, THIRTypeID},
    type_context::THIRTypeContext,
};

//
// After the evaluation and completion of the MIRUnit, this struct contains all necessary context to interpret
// the complete meaning of its contents. For instance, prototypes are not necessary to provide here as a map as
// they are either tacked onto the function definition nodes or in the types applied to the AST nodes, however
// mapping type ids is required as later steps need to be able to interpret type definitions.
//
#[derive(Debug, Clone)]
pub struct THIRDecomposedRegistry {
    architecture: ArchitectureConfig,
    typeid_map: HashMap<THIRTypeID, THIRType>,
}

impl THIRDecomposedRegistry {
    pub fn new(architecture: ArchitectureConfig, typeid_map: HashMap<THIRTypeID, THIRType>) -> Self {
        Self {
            architecture,
            typeid_map,
        }
    }
}

impl THIRTypeContext for THIRDecomposedRegistry {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn resolve_type_id(&self, id: THIRTypeID) -> &THIRType {
        self.typeid_map
            .get(&id)
            .unwrap_or_else(|| panic!("Invalid id {id} in MIRDecomposedRegistry!"))
    }
}
