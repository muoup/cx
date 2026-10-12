use std::collections::{HashSet, VecDeque};

use cx_hmir::HMIRDefID;

///
/// Tracks the definitions that the unit has to generate. A definition is requested by whatever refers to it, at
/// most once, and is generated only after the definition being lowered at the time of the request has finished.
///
#[derive(Debug, Default)]
pub struct GenerationQueue {
    requested: HashSet<HMIRDefID>,
    pending: VecDeque<HMIRDefID>,
}

impl GenerationQueue {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn request(&mut self, def: HMIRDefID) {
        if self.requested.insert(def) {
            self.pending.push_back(def);
        }
    }

    pub fn next(&mut self) -> Option<HMIRDefID> {
        self.pending.pop_front()
    }
}
