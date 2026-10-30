use rustc_hash::FxHashMap;
use std::cell::Cell;

use cx_target::ArchitectureConfig;
use index_vec::IndexVec;

use crate::ty::{
    interface::MTRegistry,
    layout::{MIRTypeLayout, compute_layout},
};

use super::{MIRType, MIRTypeID};

#[derive(Debug, Clone, Copy)]
enum LayoutState {
    Pending,
    Active,
    Ready(MIRTypeLayout),
}

#[derive(Debug, Clone)]
pub struct MIRTypeRegistry {
    architecture: ArchitectureConfig,
    definitions: IndexVec<MIRTypeID, MIRType>,
    debug_names: FxHashMap<MIRTypeID, String>,
    layouts: IndexVec<MIRTypeID, Cell<LayoutState>>,
}

impl MIRTypeRegistry {
    pub fn new(
        architecture: ArchitectureConfig,
        definitions: IndexVec<MIRTypeID, MIRType>,
        debug_names: FxHashMap<MIRTypeID, String>,
    ) -> Self {
        let layouts = definitions
            .iter()
            .map(|_| Cell::new(LayoutState::Pending))
            .collect();
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
        match self.layouts[ty].get() {
            LayoutState::Ready(layout) => return layout,
            LayoutState::Active => panic!("recursive value type {ty}"),
            LayoutState::Pending => {}
        }
        self.layouts[ty].set(LayoutState::Active);
        let layout = compute_layout(self, ty, &mut |child| self.layout(child));
        self.layouts[ty].set(LayoutState::Ready(layout));
        layout
    }

    fn debug_name(&self, id: MIRTypeID) -> Option<&str> {
        self.debug_names.get(&id).map(|s| s.as_str())
    }
}
