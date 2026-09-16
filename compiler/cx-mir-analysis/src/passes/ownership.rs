use std::collections::HashSet;

use crate::framework::environment::AnalysisEnvironment;
use crate::framework::pipeline::AnalysisPass;
use crate::framework::state::{LatticeState, Mergeable, StateTable};
use cx_log::CXResult;

use cx_mir::{MIRBasicBlockID, MIRInstruction, MIRPlace, MIRValue};

mod log;
mod state;

pub struct Ownership {
    nodrop: HashSet<MIRPlace>,
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
    fn analyze_instruction(
        &mut self,
        env: &AnalysisEnvironment,
        instruction: &MIRInstruction,
    ) -> CXResult<()> {
        let operands: Vec<&MIRValue> = todo!();

        for operand in operands {}
    }

    fn merge(&mut self, env: &AnalysisEnvironment, other: MIRBasicBlockID) {
        self.table.merge(other);
    }

    fn reload_block(&mut self, env: &AnalysisEnvironment, block: MIRBasicBlockID) {
        self.table.reload_block(block);
    }
}

impl Mergeable for OwnershipState {
    type Context = Ownership;

    fn merge(
        &mut self,
        context: &Ownership,
        other: Self,
        place: MIRPlace,
    ) -> CXResult<LatticeState<Self>> {
        Ok(match (self, other) {
            (_, _) if *self == other => LatticeState::Known(*self),

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
