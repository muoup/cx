use std::collections::HashMap;

use cx_log::{CXResult, catalogue::analysis};
use cx_mir::{
    MIRBindable, MIRComptimeBody, MIRComptimeParameter, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::log::comptime_error;

#[derive(Clone, Copy, PartialEq, Eq)]
enum State {
    Available,
    Moved,
    Uninitialized,
}

pub(crate) struct Liveness {
    function: String,
    states: HashMap<MIRBindable, State>,
}

impl Liveness {
    pub(crate) fn new(body: &MIRComptimeBody<'_>, function: &str) -> Self {
        let mut states = HashMap::new();
        for place in body.places() {
            states.insert(MIRBindable::Place(place.id), State::Uninitialized);
        }
        for register in body.registers() {
            states.insert(MIRBindable::Register(register.id), State::Uninitialized);
        }
        for parameter in body.comptime_parameters() {
            if let MIRComptimeParameter::Runtime(place) = parameter {
                states.insert(MIRBindable::Place(*place), State::Available);
            }
        }
        Self {
            function: function.to_owned(),
            states,
        }
    }

    fn name(body: &MIRComptimeBody<'_>, bindable: &MIRBindable) -> (String, bool) {
        let debug_name = match bindable {
            MIRBindable::Place(id) => body.place(*id).and_then(|place| place.debug_name.as_ref()),
            MIRBindable::Register(id) => body
                .register(*id)
                .and_then(|register| register.debug_name.as_ref()),
        };
        (
            debug_name
                .map(ToString::to_string)
                .unwrap_or_else(|| format!("{bindable:?}")),
            debug_name.is_some_and(|name| name.as_str() == "_"),
        )
    }

    pub(crate) fn require(
        &self,
        body: &MIRComptimeBody<'_>,
        bindable: &MIRBindable,
        operation: &str,
        range: &TokenRange,
    ) -> CXResult<()> {
        let state = self.states.get(bindable);
        if state == Some(&State::Available) {
            return Ok(());
        }
        let (name, discarded) = Self::name(body, bindable);
        let args = (self.function.clone(), name, operation.to_owned(), discarded);
        match state {
            Some(State::Moved) => comptime_error(range.clone(), (&analysis::AFTER_MOVE, args)),
            _ => comptime_error(range.clone(), (&analysis::BEFORE_INITIALIZATION, args)),
        }
    }

    pub(crate) fn initialize(&mut self, bindable: MIRBindable) {
        self.states.insert(bindable, State::Available);
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
                && self.states.get(bindable) == Some(&State::Available)
                && body.place(*id).is_some_and(|place| place.nodrop)
            {
                let (name, discarded) = Self::name(body, bindable);
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
            self.states.insert(bindable.clone(), State::Uninitialized);
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
            self.states.insert(bindable.clone(), State::Moved);
        }
        Ok(())
    }
}
