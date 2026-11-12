use cx_hmir::{body::HMIRBodyID, ty::context::HMIRTypeContext};
use cx_pipeline_data::hmir::facts::{HMIRTypeFacts, HMIRTypeSubject};

use crate::{env::HMIREnvironment, eval::types, value::HMIRValue};

#[derive(Debug, Default)]
pub(crate) struct TypeRecorder {
    facts: HMIRTypeFacts,
    bodies: Vec<HMIRBodyID>,
}

impl HMIREnvironment<'_, '_> {
    // Only these bodies record what they evaluate to, so template instances and code spliced in from elsewhere
    // keep their declared form
    pub(crate) fn record_bodies(&mut self, bodies: Vec<HMIRBodyID>) {
        if let Some(recorder) = &mut self.recorder {
            recorder.bodies = bodies;
        }
    }

    pub(crate) fn record_type(&mut self, subject: HMIRTypeSubject, value: &HMIRValue) {
        let recording = self
            .recorder
            .as_ref()
            .is_some_and(|recorder| recorder.bodies.contains(&subject.body()));
        if !recording {
            return;
        }
        let ty = types::name(self, self.referent(value.ty()));
        if let Some(recorder) = &mut self.recorder {
            recorder.facts.insert(subject, ty);
        }
    }

    pub fn take_type_facts(&mut self) -> Option<HMIRTypeFacts> {
        self.recorder.take().map(|recorder| recorder.facts)
    }
}
