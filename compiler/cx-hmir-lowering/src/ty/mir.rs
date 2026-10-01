use std::collections::HashMap;

use cx_mir::{
    MIRType, MIRTypeID, MIRTypeKind,
    ty::{interface::MTRegistry, registry::MIRTypeRegistry},
};
use cx_target::ArchitectureConfig;

pub(crate) struct MIRTypes {
    architecture: ArchitectureConfig,
    definitions: Vec<Option<MIRType>>,
    interner: Vec<(MIRType, MIRTypeID)>,
    debug_names: HashMap<MIRTypeID, String>,
}

impl MIRTypes {
    pub(crate) fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            architecture,
            definitions: Vec::new(),
            interner: Vec::new(),
            debug_names: HashMap::new(),
        }
    }

    pub(crate) fn intern(&mut self, ty: MIRType) -> MIRTypeID {
        if let Some(id) = self.find(&ty) {
            return id;
        }
        let id = MIRTypeID::new(self.definitions.len());
        self.definitions.push(Some(ty.clone()));
        self.interner.push((ty, id));
        id
    }

    pub(crate) fn reserve(&mut self) -> MIRTypeID {
        self.definitions.push(None);
        MIRTypeID::new(self.definitions.len() - 1)
    }

    pub(crate) fn define(&mut self, id: MIRTypeID, ty: MIRType) {
        self.definitions[id.index()] = Some(ty.clone());
        if self.find(&ty).is_none() {
            self.interner.push((ty, id));
        }
    }

    pub(crate) fn set_debug_name(&mut self, id: MIRTypeID, name: String) {
        self.debug_names.insert(id, name);
    }

    pub(crate) fn finish(self) -> MIRTypeRegistry {
        let definitions = self
            .definitions
            .into_iter()
            .enumerate()
            .map(|(index, ty)| (MIRTypeID::new(index), ty.unwrap_or_else(MIRType::undefined)))
            .collect();
        MIRTypeRegistry::new(self.architecture, definitions, self.debug_names)
    }
}

impl MTRegistry for MIRTypes {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn definition(&self, id: MIRTypeID) -> Option<&MIRType> {
        self.definitions.get(id.index()).and_then(Option::as_ref)
    }

    fn find(&self, ty: &MIRType) -> Option<MIRTypeID> {
        self.interner
            .iter()
            .find_map(|(candidate, id)| (candidate == ty).then_some(*id))
    }

    fn find_kind(&self, kind: &MIRTypeKind) -> Option<MIRTypeID> {
        self.interner
            .iter()
            .find_map(|(ty, id)| (ty.kind() == kind).then_some(*id))
    }

    fn debug_name(&self, id: MIRTypeID) -> Option<&str> {
        self.debug_names.get(&id).map(String::as_str)
    }
}
