use std::fmt::{self, Formatter};

use crate::{
    body::{HMIRBody, HMIRHoleID, HMIRLocalID},
    unit::HMIRUnit,
};

impl fmt::Display for HMIRLocalID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.index())
    }
}

impl fmt::Display for HMIRHoleID {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "?{}", self.index())
    }
}

#[derive(Clone, Copy)]
pub(super) struct BodyPrinter<'a> {
    unit: &'a HMIRUnit,
    body: &'a HMIRBody,
}

impl<'a> BodyPrinter<'a> {
    pub(super) fn new(unit: &'a HMIRUnit, body: &'a HMIRBody) -> Self {
        Self { unit, body }
    }

    pub(super) fn unit(&self) -> &'a HMIRUnit {
        self.unit
    }

    pub(super) fn body(&self) -> &'a HMIRBody {
        self.body
    }

    pub(super) fn local(&self, id: HMIRLocalID) -> String {
        let local = self.body.local(id);
        let sigil = if local.comptime() { "$" } else { "%" };
        match local.name() {
            Some(name) => format!("{sigil}{name}"),
            None => format!("{sigil}{}", id.index()),
        }
    }

    pub(super) fn local_decl(&self, id: HMIRLocalID) -> String {
        format!(
            "{}: {}",
            self.local(id),
            self.inline(self.body.local(id).ty())
        )
    }

    pub(super) fn param_decl(&self, id: HMIRLocalID) -> String {
        match self.body.local(id).comptime() {
            true => format!("comptime {}", self.local_decl(id)),
            false => self.local_decl(id),
        }
    }
}
