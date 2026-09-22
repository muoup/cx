use std::collections::HashSet;

use crate::framework::environment::AnalysisEnvironment;
use crate::framework::pipeline::AnalysisPass;
use crate::framework::state::{LatticeState, Mergeable, StateTable};
use cx_log::CXResult;

use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_mir::{MIRBasicBlockID, MIRBindable, MIRInstruction, MIRInstructionKind, MIRPlaceID};

pub struct Ownership {
    nodrop: HashSet<MIRPlaceID>,
    table: StateTable<MIRBindable, OwnershipState>,
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
    fn function_entry(&mut self, _: &AnalysisEnvironment) -> CXResult<()> {
        Ok(())
    }

    fn analyze_instruction(
        &mut self,
        env: &AnalysisEnvironment,
        instruction: &MIRInstruction,
    ) -> CXResult<()> {
        match &instruction.kind {
            MIRInstructionKind::Initialize { place, .. } => {
                self.table.set(
                    place.clone(),
                    LatticeState::Known(OwnershipState::Available),
                );
            }

            MIRInstructionKind::Invalidate { place, kind } => {
                if let MIRBindable::Place(place) = place {
                    let nodrop = self.nodrop.contains(place);

                    if nodrop && *kind == MIRInvalidationKind::Drop {
                        todo!("Nodrop error message");
                    }
                }

                let Some(state) = self.table.get(place) else {
                    unreachable!("Invalid ownership state for place {place:?}");
                };

                if *kind != MIRInvalidationKind::Drop && *state != LatticeState::Known(OwnershipState::Available) {
                    todo!(
                        "Use of uninitialized value error message in function {}, found: {state:?}",
                        env.function().prototype().symbol_name
                    );
                }

                self.table
                    .set(place.clone(), LatticeState::Known(OwnershipState::Moved));
            }

            _ => {}
        }

        Ok(())
    }

    fn merge(&mut self, env: &AnalysisEnvironment, other: MIRBasicBlockID) -> CXResult<bool> {
        let mut table = std::mem::replace(&mut self.table, StateTable::new());
        let result = table.merge_into(env, self, other)?;
        self.table = table;

        Ok(result)
    }

    fn reload_block(&mut self, _: &AnalysisEnvironment, block: MIRBasicBlockID) -> CXResult<()> {
        self.table.reload_block(block);

        Ok(())
    }
}

impl Mergeable<MIRBindable> for OwnershipState {
    type Context = Ownership;

    fn merge(
        &mut self,
        context: &Ownership,
        other: &Self,
        key: MIRBindable,
    ) -> CXResult<LatticeState<MIRBindable, Self>> {
        Ok(match (self.clone(), other) {
            (_, _) if self == other => LatticeState::Known(*self),

            (OwnershipState::Uninitialized, OwnershipState::Moved)
            | (OwnershipState::Moved, OwnershipState::Uninitialized) => {
                LatticeState::Known(OwnershipState::Uninitialized)
            }

            (OwnershipState::Available, _) | (_, OwnershipState::Available) => {
                if let MIRBindable::Place(place) = key {
                    if context.nodrop.contains(&place) {
                        todo!("Nodrop error message");
                    }
                }

                LatticeState::Known(OwnershipState::Moved)
            }

            _ => LatticeState::Top,
        })
    }
}
