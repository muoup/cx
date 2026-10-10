use std::collections::{BTreeMap, HashMap};

use cx_hmir::HMIRGlobal;
use cx_mir::{
    MIRFnPrototype, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalVariable, MIRType, MIRTypeID, MIRUnit, ty::registry::MIRTypeRegistry,
};
use cx_target::ArchitectureConfig;

///
/// Handles the internals of generating an MIRUnit via an API compatible with HMIR lowering. Used as an internal component
/// of the HMIREnvironment.
///
/// Responsible for generating MIR functions, globals, and the type registry.
///
#[derive(Debug)]
pub struct MIRModuleBuilder {
    architecture: ArchitectureConfig,

    types: HashMap<MIRTypeID, MIRType>,
    functions: BTreeMap<MIRFunctionID, MIRFunction>,
    globals: BTreeMap<MIRGlobalID, MIRGlobalVariable>,
    global_order: Vec<MIRGlobalID>,
}

impl MIRModuleBuilder {
    pub fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            architecture,

            types: HashMap::new(),
            functions: BTreeMap::new(),
            globals: BTreeMap::new(),
            global_order: Vec::new(),
        }
    }

    pub fn resolve_type(&self, ty: MIRTypeID) -> &MIRType {
        self.types.get(&ty).expect("Type not found in registry")
    }

    pub fn finish(self) -> MIRUnit {
        MIRUnit::new(
            MIRTypeRegistry::new(self.architecture, self.types, HashMap::new()),
            self.functions,
            self.globals,
            self.global_order,
        )
    }
}