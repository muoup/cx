use std::{collections::BTreeMap, rc::Rc};

use cx_mir::{
    MIRAggregateOp, MIRFunction, MIRInstrKind, MIRParameterID, MIRPlace, MIRPlaceAggregateOp,
    MIRPlaceID,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum PlaceState {
    Uninitialized,
    Available,
    Moved,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct OwnershipState {
    pub(super) places: Vec<PlaceState>,
    local_count: usize,
    projections: Rc<BTreeMap<MIRPlace, MIRPlace>>,
}

impl OwnershipState {
    pub(super) fn new(function: &MIRFunction) -> Self {
        let definition = function
            .definition()
            .expect("ownership requires a definition");
        let projections = definition
            .blocks()
            .iter()
            .flat_map(|block| block.instrs.iter())
            .filter_map(|instruction| match &instruction.kind {
                MIRInstrKind::AggregateOp(MIRAggregateOp::Place { out, op }) => {
                    let base = match op {
                        MIRPlaceAggregateOp::Field { base, .. }
                        | MIRPlaceAggregateOp::Index { base, .. }
                        | MIRPlaceAggregateOp::Variant { base, .. } => base,
                    };
                    Some((*out, *base))
                }
                _ => None,
            })
            .collect();
        let local_count = definition.places().len();
        Self {
            places: vec![
                PlaceState::Uninitialized;
                local_count + function.prototype().signature.params.len()
            ],
            local_count,
            projections: Rc::new(projections),
        }
    }

    fn index(&self, place: MIRPlace) -> Option<usize> {
        match place {
            MIRPlace::FunctionLocal(id) => Some(id.index()),
            MIRPlace::Parameter(id) => Some(self.local_count + id.index()),
            MIRPlace::Global(_) => None,
        }
    }

    pub(super) fn place(&self, index: usize) -> MIRPlace {
        if index < self.local_count {
            MIRPlace::FunctionLocal(MIRPlaceID::new(index))
        } else {
            MIRPlace::Parameter(MIRParameterID::new(index - self.local_count))
        }
    }

    pub(super) fn get(&self, place: &MIRPlace) -> Option<&PlaceState> {
        self.index(*place).and_then(|index| self.places.get(index))
    }

    pub(super) fn insert(&mut self, place: MIRPlace, state: PlaceState) {
        if let Some(index) = self.index(place) {
            self.places[index] = state;
        }
    }

    pub(super) fn remove(&mut self, place: &MIRPlace) {
        self.insert(*place, PlaceState::Uninitialized);
    }

    pub(super) fn mark_moved(&mut self, place: MIRPlace) {
        self.insert(place, PlaceState::Moved);
        let mut current = place;
        while let Some(base) = self.projections.get(&current).copied() {
            self.insert(base, PlaceState::Moved);
            current = base;
        }
    }
}
