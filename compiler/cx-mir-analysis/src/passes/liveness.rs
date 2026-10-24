use cx_log::{CXMaybeRawResult, CXResult, catalogue::analysis};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRBody, MIRInstruction, MIRInstructionKind,
    MIRInternalIntrinsic, MIRIntrinsic, MIRLivenessState, MIRPlaceID, MIRRegisterID, MIRTarget,
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

fn debug_name(body: &MIRBody, bindable: &MIRBindable) -> (String, bool) {
    let debug_name = match bindable {
        MIRBindable::Place(id) => body.place(*id).debug_name(),
        MIRBindable::Register(id) => body.register(*id).debug_name(),
    };
    let name = debug_name
        .map(ToString::to_string)
        .unwrap_or_else(|| format!("{bindable:?}"));
    let discarded = debug_name.is_some_and(|name| name.as_str() == "_");

    (name, discarded)
}

fn nodrop(env: &AnalysisEnvironment, bindable: &MIRBindable) -> bool {
    match bindable {
        MIRBindable::Place(id) => env.body().place(*id).nodrop(),
        MIRBindable::Register(_) => false,
    }
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

    fn available(&self, _: &AnalysisEnvironment, bindable: &MIRBindable) -> bool {
        matches!(self.get(bindable), Some(MIRLivenessState::Available))
    }

    fn require_available(
        &self,
        env: &AnalysisEnvironment,
        bindable: &MIRBindable,
        operation: &str,
        range: &TokenRange,
    ) -> CXResult<()> {
        let state = self.get(bindable);
        if self.available(env, bindable) {
            return Ok(());
        }

        let (name, discarded) = debug_name(env.body(), bindable);
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
        if nodrop(env, &bindable)
            && (existing == MIRLivenessState::Available || incoming == MIRLivenessState::Available)
        {
            let (name, discarded) = debug_name(env.body(), &bindable);
            return Err(analysis::PARTIAL_MOVE
                .bind((
                    env.function().prototype().display_name().to_string(),
                    name,
                    discarded,
                ))
                .into());
        }

        Ok(match (existing, incoming) {
            (MIRLivenessState::Moved, _) | (_, MIRLivenessState::Moved) => {
                MIRLivenessState::Moved
            }
            (MIRLivenessState::Uninitialized, _) | (_, MIRLivenessState::Uninitialized) => {
                MIRLivenessState::Uninitialized
            }
            _ => MIRLivenessState::Available,
        })
    }
}

impl AnalysisPass for Liveness {
    fn function_entry(&mut self, env: &AnalysisEnvironment) -> CXResult<()> {
        let body = env.body();
        self.register_count = body.registers().count();
        self.table.reset(
            MIRLivenessState::Uninitialized,
            self.register_count + body.places().count(),
            body.blocks().count(),
        );
        Ok(())
    }

    fn analyze_instruction(
        &mut self,
        env: &AnalysisEnvironment,
        instruction: &MIRInstruction,
    ) -> CXResult<()> {
        let mut unavailable = None;
        let addresses = matches!(
            instruction.kind,
            MIRInstructionKind::IntrinsicOp(MIRIntrinsic::Internal(
                MIRInternalIntrinsic::PlaceAddress { .. }
            ))
        );
        if !addresses {
            visit_bindable_uses(&instruction.kind, |bindable| {
                if unavailable.is_none() && !self.available(env, &bindable) {
                    unavailable = Some(bindable);
                }
            });
        }
        if let Some(bindable) = unavailable {
            self.require_available(env, &bindable, "was used", &instruction.token_range)?;
        }

        match &instruction.kind {
            MIRInstructionKind::Initialize { place } => {
                self.set(place, MIRLivenessState::Available);
            }
            MIRInstructionKind::Invalidate { place, kind } => {
                if *kind == MIRInvalidationKind::Drop {
                    if self.get(place) == Some(MIRLivenessState::Available) && nodrop(env, place) {
                        let (name, discarded) = debug_name(env.body(), place);
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
            MIRInstructionKind::Lift { out, origin, .. } => {
                self.require_available(env, origin, "was read", &instruction.token_range)?;
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
