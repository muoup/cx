use std::cell::Cell;

use index_vec::IndexVec;

use crate::ty::{
    MIRTypeID,
    interface::MTRegistry,
    layout::{MIRTypeLayout, compute_layout},
};

#[derive(Debug, Clone, Copy)]
enum LayoutState {
    Pending,
    Active,
    Ready(MIRTypeLayout),
}

// Only defined types are ever laid out, so a layout is never computed for a type its definition later completes
#[derive(Debug, Clone, Default)]
pub struct MIRLayoutCache {
    states: IndexVec<MIRTypeID, Cell<LayoutState>>,
}

impl MIRLayoutCache {
    pub fn new(types: usize) -> Self {
        Self {
            states: (0..types).map(|_| Cell::new(LayoutState::Pending)).collect(),
        }
    }

    pub fn track(&mut self) {
        self.states.push(Cell::new(LayoutState::Pending));
    }

    pub fn reset(&mut self, ty: MIRTypeID) {
        self.states[ty].set(LayoutState::Pending);
    }

    pub fn layout<Registry: MTRegistry>(&self, registry: &Registry, ty: MIRTypeID) -> MIRTypeLayout {
        match self.states[ty].get() {
            LayoutState::Ready(layout) => return layout,
            LayoutState::Active => panic!("recursive value type {ty}"),
            LayoutState::Pending => {}
        }
        self.states[ty].set(LayoutState::Active);
        let layout = compute_layout(registry, ty, &mut |child| self.layout(registry, child));
        self.states[ty].set(LayoutState::Ready(layout));
        layout
    }
}
