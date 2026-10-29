use cx_mir::MIRBindable;

use crate::value::HMIRValue;

#[derive(Debug)]
pub(crate) struct Slot {
    binding: MIRBindable,
    known: Option<HMIRValue>,
}

impl Slot {
    pub(crate) fn new(binding: MIRBindable, known: Option<HMIRValue>) -> Self {
        Self { binding, known }
    }

    pub(crate) fn binding(&self) -> &MIRBindable {
        &self.binding
    }

    pub(crate) fn known(&self) -> Option<&HMIRValue> {
        self.known.as_ref()
    }

    pub(crate) fn set_known(&mut self, known: Option<HMIRValue>) {
        self.known = known;
    }
}
