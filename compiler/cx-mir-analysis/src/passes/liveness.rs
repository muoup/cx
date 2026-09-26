use cx_log::{CXMaybeRawResult, CXResult, catalogue::analysis};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRInstruction, MIRInstructionKind, MIRLivenessState, MIRPlaceID,
    MIRRegisterID, MIRTarget,
    expr::{instruction::MIRInvalidationKind, visit::visit_bindable_uses},
};
use cx_tokens::TokenRange;

use crate::{
    framework::{environment::AnalysisEnvironment, pipeline::AnalysisPass, state::StateTable},
    log::{complete_analysis_error, log_analysis_error},
};

pub struct Liveness {
    table: StateTable<MIRLivenessState>,

    // Registers occupy the first `register_count` table slots, places the rest
    register_count: usize,
}

impl Liveness {
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

    fn get(&self, bindable: &MIRBindable) -> Option<MIRLivenessState> {
        self.table.get(self.key(bindable))
    }

    fn set(&mut self, bindable: &MIRBindable, state: MIRLivenessState) {
        self.table.set(self.key(bindable), state);
    }

    fn require_available(
        &self,
        env: &AnalysisEnvironment,
        bindable: &MIRBindable,
        operation: &str,
        range: &TokenRange,
    ) -> CXResult<()> {
        let state = self.get(bindable);
        if state == Some(MIRLivenessState::Available) {
            return Ok(());
        }

        let (name, discarded) = env.body().bindable_debug_name(bindable);
        let args = (
            env.function().prototype().display_name().to_string(),
            name,
            operation.to_owned(),
            discarded,
        );
        match state {
            Some(MIRLivenessState::Moved) => log_analysis_error(range, (&analysis::AFTER_MOVE, args)),
            _ => log_analysis_error(range, (&analysis::BEFORE_INITIALIZATION, args)),
        }
    }

    fn join(
        env: &AnalysisEnvironment,
        bindable: MIRBindable,
        existing: MIRLivenessState,
        incoming: MIRLivenessState,
    ) -> CXMaybeRawResult<MIRLivenessState> {
        if let MIRBindable::Place(place) = bindable
            && env.body().place(place).is_some_and(|place| place.nodrop)
            && (existing == MIRLivenessState::Available || incoming == MIRLivenessState::Available)
        {
            let (name, discarded) = env.body().bindable_debug_name(&bindable);
            return Err(analysis::PARTIAL_MOVE
                .bind((
                    env.function().prototype().display_name().to_string(),
                    name,
                    discarded,
                ))
                .into());
        }

        Ok(match (existing, incoming) {
            (MIRLivenessState::Uninitialized, _) | (_, MIRLivenessState::Uninitialized) => {
                MIRLivenessState::Uninitialized
            }
            _ => MIRLivenessState::Moved,
        })
    }
}

impl AnalysisPass for Liveness {
    fn function_entry(&mut self, env: &AnalysisEnvironment) -> CXResult<()> {
        let body = env.body();
        self.register_count = body.registers().len();
        self.table.reset(
            MIRLivenessState::Uninitialized,
            body.registers().len() + body.places().len(),
            body.blocks().len(),
        );
        Ok(())
    }

    fn block_entry(&mut self, env: &AnalysisEnvironment, block: MIRBasicBlockID) -> CXResult<()> {
        if let Some(block) = env.body().block(block) {
            for register in block.params() {
                self.set(&MIRBindable::Register(*register), MIRLivenessState::Available);
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
            if unavailable.is_none() && self.get(&bindable) != Some(MIRLivenessState::Available) {
                unavailable = Some(bindable);
            }
        });
        if let Some(bindable) = unavailable {
            self.require_available(env, &bindable, "was used", &instruction.token_range)?;
        }

        match &instruction.kind {
            MIRInstructionKind::Initialize { place } => {
                self.set(place, MIRLivenessState::Available);
            }
            MIRInstructionKind::Invalidate { place, kind } => {
                if *kind == MIRInvalidationKind::Drop {
                    if self.get(place) == Some(MIRLivenessState::Available) {
                        if let MIRBindable::Place(id) = place {
                            if env.body().place(*id).is_some_and(|place| place.nodrop) {
                                let (name, discarded) = env.body().bindable_debug_name(place);
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
                    self.set(place, MIRLivenessState::Uninitialized);
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
                    self.set(place, MIRLivenessState::Moved);
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
                self.set(&MIRBindable::Register(*out), MIRLivenessState::Available);
            }
            MIRInstructionKind::Store {
                target: MIRTarget::Register(out),
                ..
            } => {
                self.set(&MIRBindable::Register(*out), MIRLivenessState::Available);
            }
            MIRInstructionKind::Call { out: Some(out), .. } => {
                self.set(&MIRBindable::Register(*out), MIRLivenessState::Available);
            }
            MIRInstructionKind::IntrinsicOp(op) => {
                if let Some(MIRTarget::Register(out)) = op.output_target() {
                    self.set(&MIRBindable::Register(out), MIRLivenessState::Available);
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
