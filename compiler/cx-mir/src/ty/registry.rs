use rustc_hash::FxHashMap;

use cx_target::ArchitectureConfig;
use index_vec::IndexVec;

use crate::ty::{
    interface::MTRegistry,
    layout::{MIRTypeLayout, cache::MIRLayoutCache},
};

use super::{MIRType, MIRTypeID};

#[derive(Debug, Clone)]
pub struct MIRTypeRegistry {
    architecture: ArchitectureConfig,
    definitions: IndexVec<MIRTypeID, MIRType>,
    debug_names: FxHashMap<MIRTypeID, String>,
    layouts: MIRLayoutCache,
}

impl MIRTypeRegistry {
    pub fn new(
        architecture: ArchitectureConfig,
        definitions: IndexVec<MIRTypeID, MIRType>,
        debug_names: FxHashMap<MIRTypeID, String>,
    ) -> Self {
        let layouts = MIRLayoutCache::new(definitions.len());
        Self {
            architecture,
            definitions,
            debug_names,
            layouts,
        }
    }
}

impl MTRegistry for MIRTypeRegistry {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn definition(&self, id: MIRTypeID) -> Option<&MIRType> {
        self.definitions.get(id)
    }

    fn find(&self, ty: &MIRType) -> Option<MIRTypeID> {
        self.definitions
            .iter_enumerated()
            .find_map(|(id, definition)| (definition == ty).then_some(id))
    }

    fn find_kind(&self, kind: &super::MIRTypeKind) -> Option<MIRTypeID> {
        self.definitions
            .iter_enumerated()
            .find_map(|(id, definition)| (definition.kind() == kind).then_some(id))
    }

    fn layout(&self, ty: MIRTypeID) -> MIRTypeLayout {
        self.layouts.layout(self, ty)
    }

    fn debug_name(&self, id: MIRTypeID) -> Option<&str> {
        self.debug_names.get(&id).map(|s| s.as_str())
    }
}
