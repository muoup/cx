use cx_log::{CXMaybeRawResult, CXResult, catalogue::analysis};
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRInstruction, MIRInstructionKind, MIRTarget,
    expr::{instruction::MIRInvalidationKind, visit::visit_bindable_uses},
};
use cx_tokens::TokenRange;

use crate::{
    framework::{
        environment::AnalysisEnvironment,
        pipeline::AnalysisPass,
        state::{LatticeState, Mergeable, StateTable},
    },
    log::{complete_analysis_error, log_analysis_error},
};

pub struct Ownership {
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
            table: StateTable::new(),
        }
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
        let state = self.table.get(bindable);
        if state == Some(&LatticeState::Known(OwnershipState::Available)) {
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
            Some(LatticeState::Known(OwnershipState::Moved)) => {
                log_analysis_error(range, (&analysis::AFTER_MOVE, args))
            }
            _ => log_analysis_error(range, (&analysis::BEFORE_INITIALIZATION, args)),
        }
    }
}

impl AnalysisPass for Ownership {
    fn function_entry(&mut self, env: &AnalysisEnvironment) -> CXResult<()> {
        let body = env.function().body().expect("analyzed function has a body");
        for place in body.places() {
            self.table.set(
                MIRBindable::Place(place.id),
                LatticeState::Known(OwnershipState::Uninitialized),
            );
        }
        for register in body.registers() {
            self.table.set(
                MIRBindable::Register(register.id),
                LatticeState::Known(OwnershipState::Uninitialized),
            );
        }
        Ok(())
    }

    fn block_entry(&mut self, env: &AnalysisEnvironment, block: MIRBasicBlockID) -> CXResult<()> {
        let body = env.function().body().expect("analyzed function has a body");
        if let Some(block) = body.block(block) {
            for register in block.params() {
                self.table.set(
                    MIRBindable::Register(*register),
                    LatticeState::Known(OwnershipState::Available),
                );
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
            if unavailable.is_none()
                && self.table.get(&bindable)
                    != Some(&LatticeState::Known(OwnershipState::Available))
            {
                unavailable = Some(bindable);
            }
        });
        if let Some(bindable) = unavailable {
            self.require_available(env, &bindable, "was used", &instruction.token_range)?;
        }

        match &instruction.kind {
            MIRInstructionKind::Initialize { place } => {
                self.table.set(
                    place.clone(),
                    LatticeState::Known(OwnershipState::Available),
                );
            }
            MIRInstructionKind::Invalidate { place, kind } => {
                if *kind == MIRInvalidationKind::Drop {
                    if self.table.get(place)
                        == Some(&LatticeState::Known(OwnershipState::Available))
                    {
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
                    self.table.set(
                        place.clone(),
                        LatticeState::Known(OwnershipState::Uninitialized),
                    );
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
                    self.table
                        .set(place.clone(), LatticeState::Known(OwnershipState::Moved));
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
                self.table.set(
                    MIRBindable::Register(*out),
                    LatticeState::Known(OwnershipState::Available),
                );
            }
            MIRInstructionKind::Store {
                target: MIRTarget::Register(out),
                ..
            } => {
                self.table.set(
                    MIRBindable::Register(*out),
                    LatticeState::Known(OwnershipState::Available),
                );
            }
            MIRInstructionKind::Call { out: Some(out), .. } => {
                self.table.set(
                    MIRBindable::Register(*out),
                    LatticeState::Known(OwnershipState::Available),
                );
            }
            MIRInstructionKind::IntrinsicOp(op) => {
                if let Some(MIRTarget::Register(out)) = op.output_target() {
                    self.table.set(
                        MIRBindable::Register(out),
                        LatticeState::Known(OwnershipState::Available),
                    );
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
        let mut table = std::mem::replace(&mut self.table, StateTable::new());
        let result = table
            .merge_into(env, other)
            .map_err(|err| complete_analysis_error(range, err));
        self.table = table;
        result
    }

    fn reload_block(&mut self, _: &AnalysisEnvironment, block: MIRBasicBlockID) -> CXResult<()> {
        self.table.reload_block(block);
        Ok(())
    }
}

impl Mergeable<MIRBindable> for OwnershipState {
    fn merge(
        &self,
        env: &AnalysisEnvironment,
        other: &Self,
        key: MIRBindable,
    ) -> CXMaybeRawResult<Option<LatticeState<MIRBindable, Self>>> {
        if self == other {
            return Ok(None);
        }

        if let MIRBindable::Place(place) = key {
            if env
                .function()
                .body()
                .and_then(|body| body.place(place))
                .is_some_and(|place| place.nodrop)
                && (self == &OwnershipState::Available || other == &OwnershipState::Available)
            {
                let (name, discarded) = Ownership::name(env, &MIRBindable::Place(place));
                return Err(analysis::PARTIAL_MOVE
                    .bind((
                        env.function().prototype().display_name().to_string(),
                        name,
                        discarded,
                    ))
                    .into());
            }
        }

        let joined = match (self, other) {
            (OwnershipState::Uninitialized, _) | (_, OwnershipState::Uninitialized) => {
                OwnershipState::Uninitialized
            }
            _ => OwnershipState::Moved,
        };
        Ok(Some(LatticeState::Known(joined)))
    }
}
