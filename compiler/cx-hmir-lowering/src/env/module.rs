use std::collections::{BTreeMap, HashMap};

use cx_mir::{
    MIRFnPrototype, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable,
    MIRType, MIRTypeID, MIRUnit, ty::registry::MIRTypeRegistry,
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

    function_names: HashMap<String, MIRFunctionID>,
    global_names: HashMap<String, MIRGlobalID>,
}

impl MIRModuleBuilder {
    pub fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            architecture,

            types: HashMap::new(),
            functions: BTreeMap::new(),
            globals: BTreeMap::new(),
            global_order: Vec::new(),

            function_names: HashMap::new(),
            global_names: HashMap::new(),
        }
    }

    pub fn resolve_type(&self, ty: MIRTypeID) -> &MIRType {
        self.types.get(&ty).expect("Type not found in registry")
    }

    pub fn intern_type(&mut self, ty: MIRType) -> MIRTypeID {
        if let Some((id, _)) = self.types.iter().find(|(_, existing)| **existing == ty) {
            return *id;
        }

        let id = MIRTypeID::new(self.types.len());
        self.types.insert(id, ty);
        id
    }

    pub fn declare_function(&mut self, prototype: MIRFnPrototype) -> MIRFunctionID {
        let name = prototype.symbol_name.as_string();
        if let Some(id) = self.function_names.get(&name) {
            return *id;
        }

        let id = MIRFunctionID::new(self.functions.len());
        self.functions.insert(id, MIRFunction::new(prototype, None));
        self.function_names.insert(name, id);
        id
    }

    pub fn define_function(&mut self, id: MIRFunctionID, function: MIRFunction) {
        *self.functions.get_mut(&id).unwrap() = function;
    }

    pub fn function(&self, id: MIRFunctionID) -> &MIRFunction {
        &self.functions[&id]
    }

    pub fn declare_global(&mut self, global: MIRGlobalVariable) -> MIRGlobalID {
        let name = global.name().as_string();
        if let Some(id) = self.global_names.get(&name) {
            return *id;
        }

        let id = MIRGlobalID::new(self.globals.len());
        self.globals.insert(id, global);
        self.global_names.insert(name, id);
        self.global_order.push(id);
        id
    }

    pub fn define_global(&mut self, id: MIRGlobalID, state: MIRGlobalState) {
        self.globals
            .get_mut(&id)
            .unwrap_or_else(|| panic!("Could not define unknown global id: {id}"))
            .define(state);
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
