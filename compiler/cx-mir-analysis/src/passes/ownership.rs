use cx_log::{CXMaybeRawResult, CXResult, catalogue::analysis};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRInstruction, MIRInstructionKind, MIRPlaceID, MIRRegisterID,
    MIRTarget,
    expr::{instruction::MIRInvalidationKind, visit::visit_bindable_uses},
};
use cx_tokens::TokenRange;

use crate::{
    framework::{environment::AnalysisEnvironment, pipeline::AnalysisPass, state::StateTable},
    log::{complete_analysis_error, log_analysis_error},
};

pub struct Ownership {
    table: StateTable<OwnershipState>,

    // Registers occupy the first `register_count` table slots, places the rest
    register_count: usize,
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
            table: StateTable::new(),
            register_count: 0,
        }
    }

    fn key(&self, bindable: &MIRBindable) -> usize {
        match bindable {
            MIRBindable::Register(id) => id.index(),
            MIRBindable::Place(id) => self.register_count + id.index(),
        }
    }

    fn get(&self, bindable: &MIRBindable) -> Option<OwnershipState> {
        self.table.get(self.key(bindable))
    }

    fn set(&mut self, bindable: &MIRBindable, state: OwnershipState) {
        self.table.set(self.key(bindable), state);
    }

    fn name(env: &AnalysisEnvironment, bindable: &MIRBindable) -> (String, bool) {
        let body = env.function().body().expect("analyzed function has a body");
        let debug_name = match bindable {
            MIRBindable::Place(id) => body.place(*id).and_then(|place| place.debug_name.as_ref()),
            MIRBindable::Register(id) => body
                .register(*id)
                .and_then(|register| register.debug_name.as_ref()),
        };
        let name = debug_name
            .map(ToString::to_string)
            .unwrap_or_else(|| format!("{bindable:?}"));
        let discarded = debug_name.is_some_and(|name| name.as_str() == "_");
        (name, discarded)
    }

    fn require_available(
        &self,
        env: &AnalysisEnvironment,
        bindable: &MIRBindable,
        operation: &str,
        range: &TokenRange,
    ) -> CXResult<()> {
        let state = self.get(bindable);
        if state == Some(OwnershipState::Available) {
            return Ok(());
        }

        let (name, discarded) = Self::name(env, bindable);
        let args = (
            env.function().prototype().display_name().to_string(),
            name,
            operation.to_owned(),
            discarded,
        );
        match state {
            Some(OwnershipState::Moved) => log_analysis_error(range, (&analysis::AFTER_MOVE, args)),
            _ => log_analysis_error(range, (&analysis::BEFORE_INITIALIZATION, args)),
        }
    }

    fn join(
        env: &AnalysisEnvironment,
        bindable: MIRBindable,
        existing: OwnershipState,
        incoming: OwnershipState,
    ) -> CXMaybeRawResult<OwnershipState> {
        if let MIRBindable::Place(place) = bindable
            && env
                .function()
                .body()
                .and_then(|body| body.place(place))
                .is_some_and(|place| place.nodrop)
            && (existing == OwnershipState::Available || incoming == OwnershipState::Available)
        {
            let (name, discarded) = Self::name(env, &bindable);
            return Err(analysis::PARTIAL_MOVE
                .bind((
                    env.function().prototype().display_name().to_string(),
                    name,
                    discarded,
                ))
                .into());
        }

        Ok(match (existing, incoming) {
            (OwnershipState::Uninitialized, _) | (_, OwnershipState::Uninitialized) => {
                OwnershipState::Uninitialized
            }
            _ => OwnershipState::Moved,
        })
    }
}

impl AnalysisPass for Ownership {
    fn function_entry(&mut self, env: &AnalysisEnvironment) -> CXResult<()> {
        let body = env.function().body().expect("analyzed function has a body");
        self.register_count = body.registers().len();
        self.table.reset(
            OwnershipState::Uninitialized,
            body.registers().len() + body.places().len(),
            body.blocks().len(),
        );
        Ok(())
    }

    fn block_entry(&mut self, env: &AnalysisEnvironment, block: MIRBasicBlockID) -> CXResult<()> {
        let body = env.function().body().expect("analyzed function has a body");
        if let Some(block) = body.block(block) {
            for register in block.params() {
                self.set(&MIRBindable::Register(*register), OwnershipState::Available);
            }
        }
        Ok(())
    }

    fn analyze_instruction(
        &mut self,
        env: &AnalysisEnvironment,
        instruction: &MIRInstruction,
    ) -> CXResult<()> {
        let mut unavailable = None;
        visit_bindable_uses(&instruction.kind, |bindable| {
            if unavailable.is_none() && self.get(&bindable) != Some(OwnershipState::Available) {
                unavailable = Some(bindable);
            }
        });
        if let Some(bindable) = unavailable {
            self.require_available(env, &bindable, "was used", &instruction.token_range)?;
        }

        match &instruction.kind {
            MIRInstructionKind::Initialize { place } => {
                self.set(place, OwnershipState::Available);
            }
            MIRInstructionKind::Invalidate { place, kind } => {
                if *kind == MIRInvalidationKind::Drop {
                    if self.get(place) == Some(OwnershipState::Available) {
                        if let MIRBindable::Place(id) = place {
                            if env
                                .function()
                                .body()
                                .and_then(|body| body.place(*id))
                                .is_some_and(|place| place.nodrop)
                            {
                                let (name, discarded) = Self::name(env, place);
                                return log_analysis_error(
                                    &instruction.token_range,
                                    (
                                        &analysis::VALUE_NOT_CONSUMED,
                                        (
                                            env.function().prototype().display_name().to_string(),
                                            "value".to_owned(),
                                            name,
                                            "lifetime".to_owned(),
                                            discarded,
                                        ),
                                    ),
                                );
                            }
                        }
                    }
                    self.set(place, OwnershipState::Uninitialized);
                } else {
                    self.require_available(
                        env,
                        place,
                        if *kind == MIRInvalidationKind::Move {
                            "was moved"
                        } else {
                            "was leaked"
                        },
                        &instruction.token_range,
                    )?;
                    self.set(place, OwnershipState::Moved);
                }
            }
            MIRInstructionKind::Lift { out, source } => {
                match source {
                    MIRTarget::Place(place) => self.require_available(
                        env,
                        &MIRBindable::Place(*place),
                        "was read",
                        &instruction.token_range,
                    )?,
                    MIRTarget::Indirect(register) | MIRTarget::Register(register) => {
                        self.require_available(
                            env,
                            &MIRBindable::Register(*register),
                            "was read",
                            &instruction.token_range,
                        )?
                    }
                    MIRTarget::Global(_) => {}
                }
                self.set(&MIRBindable::Register(*out), OwnershipState::Available);
            }
            MIRInstructionKind::Store {
                target: MIRTarget::Register(out),
                ..
            } => {
                self.set(&MIRBindable::Register(*out), OwnershipState::Available);
            }
            MIRInstructionKind::Call { out: Some(out), .. } => {
                self.set(&MIRBindable::Register(*out), OwnershipState::Available);
            }
            MIRInstructionKind::IntrinsicOp(op) => {
                if let Some(MIRTarget::Register(out)) = op.output_target() {
                    self.set(&MIRBindable::Register(out), OwnershipState::Available);
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn merge(
        &mut self,
        env: &AnalysisEnvironment,
        other: MIRBasicBlockID,
        range: &TokenRange,
    ) -> CXResult<bool> {
        let register_count = self.register_count;
        let bindable = |key: usize| match key.checked_sub(register_count) {
            Some(place) => MIRBindable::Place(MIRPlaceID::new(place)),
            None => MIRBindable::Register(MIRRegisterID::new(key)),
        };

        self.table
            .merge_into(other, |key, existing, incoming| {
                Self::join(env, bindable(key), existing, incoming)
            })
            .map_err(|err| complete_analysis_error(range, err))
    }

    fn reload_block(&mut self, _: &AnalysisEnvironment, block: MIRBasicBlockID) -> CXResult<()> {
        self.table.reload_block(block);
        Ok(())
    }
}
