use std::collections::{HashMap, HashSet, VecDeque, hash_map::Entry};

use cx_hmir::{HMIRDefID, HMIRFnSignature, ty::HMIRTypeID};
use cx_mir::MIRFunctionID;

use crate::value::{HMIRValue, constant::HMIRConstant};

///
/// Tracks the definitions that the unit has to generate. A definition is requested by whatever refers to it, at
/// most once, and is generated only after the definition being lowered at the time of the request has finished.
///
#[derive(Debug, Default)]
pub struct GenerationQueue {
    requested: HashSet<HMIRDefID>,
    pending: VecDeque<Generation>,
    specializations: HashMap<(HMIRDefID, Vec<HMIRConstant>), MIRFunctionID>,
}

impl GenerationQueue {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn request(&mut self, def: HMIRDefID) {
        if self.requested.insert(def) {
            self.pending.push_back(Generation::Definition(def));
        }
    }

    pub fn specialization_count(&self) -> usize {
        self.specializations.len()
    }

    pub fn specialization(&self, def: HMIRDefID, args: &[HMIRConstant]) -> Option<MIRFunctionID> {
        self.specializations.get(&(def, args.to_vec())).copied()
    }

    pub fn specialize(&mut self, args: Vec<HMIRConstant>, specialization: Specialization) {
        let key = (specialization.def, args);
        if let Entry::Vacant(entry) = self.specializations.entry(key) {
            entry.insert(specialization.function());
            self.pending
                .push_back(Generation::Specialization(specialization));
        }
    }

    pub fn next(&mut self) -> Option<Generation> {
        self.pending.pop_front()
    }
}

#[derive(Debug)]
pub enum Generation {
    Definition(HMIRDefID),
    Specialization(Specialization),
}

#[derive(Debug)]
pub struct Specialization {
    def: HMIRDefID,
    function: MIRFunctionID,
    signature: HMIRFnSignature<HMIRTypeID>,
    bindings: Vec<Option<HMIRValue>>,
}

impl Specialization {
    pub fn new(
        def: HMIRDefID,
        function: MIRFunctionID,
        signature: HMIRFnSignature<HMIRTypeID>,
        bindings: Vec<Option<HMIRValue>>,
    ) -> Self {
        Self {
            def,
            function,
            signature,
            bindings,
        }
    }

    pub fn def(&self) -> HMIRDefID {
        self.def
    }
    pub fn function(&self) -> MIRFunctionID {
        self.function
    }
    pub fn signature(&self) -> &HMIRFnSignature<HMIRTypeID> {
        &self.signature
    }
    pub fn bindings(&self) -> &[Option<HMIRValue>] {
        &self.bindings
    }
}
