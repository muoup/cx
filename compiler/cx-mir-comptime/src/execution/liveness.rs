use std::collections::HashMap;

use cx_log::{CXResult, catalogue::analysis};
use cx_mir::{
    MIRBindable, MIRComptimeBody, MIRComptimeParameter, MIRLivenessState,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::log::comptime_error;

pub(crate) struct Liveness {
    function: String,
    states: HashMap<MIRBindable, MIRLivenessState>,
}

impl Liveness {
    pub(crate) fn new(body: &MIRComptimeBody<'_>, function: &str) -> Self {
        let mut states = HashMap::new();
        for place in body.places() {
            states.insert(
                MIRBindable::Place(place.id),
                MIRLivenessState::Uninitialized,
            );
        }
        for register in body.registers() {
            states.insert(
                MIRBindable::Register(register.id),
                MIRLivenessState::Uninitialized,
            );
        }
        for parameter in body.comptime_parameters() {
            if let MIRComptimeParameter::Runtime(place) = parameter {
                states.insert(MIRBindable::Place(*place), MIRLivenessState::Available);
            }
        }
        Self {
            function: function.to_owned(),
            states,
        }
    }

    pub(crate) fn require(
        &self,
        body: &MIRComptimeBody<'_>,
        bindable: &MIRBindable,
        operation: &str,
        range: &TokenRange,
    ) -> CXResult<()> {
        let state = self.states.get(bindable);
        if state == Some(&MIRLivenessState::Available) {
            return Ok(());
        }
        let (name, discarded) = body.bindable_debug_name(bindable);
        let args = (self.function.clone(), name, operation.to_owned(), discarded);
        match state {
            Some(MIRLivenessState::Moved) => {
                comptime_error(range.clone(), (&analysis::AFTER_MOVE, args))
            }
            _ => comptime_error(range.clone(), (&analysis::BEFORE_INITIALIZATION, args)),
        }
    }

    pub(crate) fn initialize(&mut self, bindable: MIRBindable) {
        self.states.insert(bindable, MIRLivenessState::Available);
    }

    pub(crate) fn invalidate(
        &mut self,
        body: &MIRComptimeBody<'_>,
        bindable: &MIRBindable,
        kind: &MIRInvalidationKind,
        range: &TokenRange,
    ) -> CXResult<()> {
        if *kind == MIRInvalidationKind::Drop {
            if let MIRBindable::Place(id) = bindable
                && self.states.get(bindable) == Some(&MIRLivenessState::Available)
                && body.place(*id).is_some_and(|place| place.nodrop)
            {
                let (name, discarded) = body.bindable_debug_name(bindable);
                return comptime_error(
                    range.clone(),
                    (
                        &analysis::VALUE_NOT_CONSUMED,
                        (
                            self.function.clone(),
                            "value".to_owned(),
                            name,
                            "lifetime".to_owned(),
                            discarded,
                        ),
                    ),
                );
            }
            self.states
                .insert(bindable.clone(), MIRLivenessState::Uninitialized);
        } else {
            self.require(
                body,
                bindable,
                if *kind == MIRInvalidationKind::Move {
                    "was moved"
                } else {
                    "was leaked"
                },
                range,
            )?;
            self.states
                .insert(bindable.clone(), MIRLivenessState::Moved);
        }
        Ok(())
    }
}
