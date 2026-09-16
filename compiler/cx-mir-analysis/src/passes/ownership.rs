use std::collections::HashSet;

use crate::framework::environment::AnalysisEnvironment;
use crate::framework::pipeline::AnalysisPass;
use crate::framework::state::{LatticeState, Mergeable, StateTable};
use cx_log::CXResult;

use cx_mir::{MIRBasicBlockID, MIRInstrKind, MIRInstruction, MIRPlaceID};

pub struct Ownership {
    nodrop: HashSet<MIRPlaceID>,
    table: StateTable<OwnershipState>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OwnershipState {
    Available,
    Moved,
    Uninitialized,
}

impl Ownership {
    pub fn new() -> Self {
        Self {
            nodrop: HashSet::new(),
            table: StateTable::new(),
        }
    }
}

impl AnalysisPass for Ownership {
    fn function_entry(&mut self, env: &AnalysisEnvironment) -> CXResult<()> {
        self.table.clear();
        self.nodrop.clear();

        for place in env.function().places() {
            if place.is_nodrop() {
                self.nodrop.insert(place.id());
            }
        }

        Ok(())
    }

    fn analyze_instruction(
        &mut self,
        env: &AnalysisEnvironment,
        instruction: &MIRInstruction,
    ) -> CXResult<()> {
        match &instruction.kind {
            MIRInstrKind::Initialize { place, .. } => {
                self.table.set(place.id(), LatticeState::Known(OwnershipState::Available));
            }

            MIRInstrKind::Invalidate { place, leak } => {
                let nodrop = self.nodrop.contains(place);

                if *leak && nodrop {
                    todo!("Nodrop error message");
                }

                self.table.set(place.id(), LatticeState::Known(OwnershipState::Moved));
            }

            _ => {}
        }

        Ok(())
    }

    fn merge(&mut self, env: &AnalysisEnvironment, other: MIRBasicBlockID) -> CXResult<()> {
        self.table.merge(env, self, other)
    }

    fn reload_block(&mut self, env: &AnalysisEnvironment, block: MIRBasicBlockID) -> CXResult<()> {
        self.table.reload_block(block);
        Ok(())
    }
}

impl Mergeable for OwnershipState {
    type Context = Ownership;

    fn merge(
        &mut self,
        context: &Ownership,
        other: &Self,
        place: MIRPlaceID,
    ) -> CXResult<LatticeState<Self>> {
        Ok(match (self, other) {
            (_, _) if *self == *other => LatticeState::Known(*self),

            (OwnershipState::Uninitialized, OwnershipState::Moved)
            | (OwnershipState::Moved, OwnershipState::Uninitialized) => {
                LatticeState::Known(OwnershipState::Uninitialized)
            }

            (OwnershipState::Available, _) | (_, OwnershipState::Available) => {
                if context.nodrop.contains(&place) {
                    todo!("Nodrop error message");
                } else {
                    LatticeState::Known(OwnershipState::Available)
                }
            }

            _ => LatticeState::Top,
        })
    }
}
