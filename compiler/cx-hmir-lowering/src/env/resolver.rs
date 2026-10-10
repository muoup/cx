use std::collections::HashMap;

use cx_hmir::ty::{
    HMIRType, HMIRTypeID, HMIRTypeKind,
    aggregate::HMIRMoveSemantics,
};
use cx_mir::MIRTypeID;
use cx_tokens::TokenRange;

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

    pub fn realize_type(&mut self, kind: HMIRTypeKind, span: &TokenRange) -> HMIRTypeID {
        let id = HMIRTypeID::new(self.realized.len() | REALIZED_BIT);
        let ty = HMIRType::new(
            id,
            None,
            kind,
            HMIRMoveSemantics::POD,
            false,
            span.clone(),
        );

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
