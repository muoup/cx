use std::collections::HashMap;

use cx_hmir::ty::{HMIRType, HMIRTypeID};
use cx_mir::MIRTypeID;

const REALIZED_BIT: usize = 1 << 63;

#[derive(Debug)]
pub(crate) struct HMIRResolver {
    realized: HashMap<HMIRTypeID, HMIRType>,
    lowered: HashMap<HMIRTypeID, MIRTypeID>,
}

impl HMIRResolver {
    pub fn new() -> Self {
        Self {
            realized: HashMap::new(),
            lowered: HashMap::new(),
        }
    }

    pub fn realize_type(&mut self, ty: HMIRType) -> HMIRTypeID {
        let id = HMIRTypeID::new(self.realized.len() | REALIZED_BIT);
        self.realized.insert(id, ty);
        id
    }

    pub fn realized_type(&self, ty: HMIRTypeID) -> Option<&HMIRType> {
        self.realized.get(&ty)
    }

    pub fn lowered_type(&self, ty: HMIRTypeID) -> Option<MIRTypeID> {
        self.lowered.get(&ty).copied()
    }

    pub fn cache_lowered(&mut self, ty: HMIRTypeID, mir: MIRTypeID) {
        self.lowered.insert(ty, mir);
    }
}
