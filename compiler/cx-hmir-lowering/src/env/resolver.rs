use cx_hmir::ty::{HMIRType, HMIRTypeID, HMIRTypeKind};
use cx_mir::MIRTypeID;
use cx_tokens::TokenRange;

#[derive(Debug)]
pub(crate) struct HMIRResolver {}

impl HMIRResolver {
    pub fn new() -> Self {
        Self {}
    }

    pub fn realize_type(&mut self, kind: HMIRTypeKind, span: &TokenRange) -> HMIRTypeID {
        todo!()
    }

    pub fn realized_type(&self, ty: HMIRTypeID) -> Option<&HMIRType> {
        todo!()
    }

    pub fn lowered_type(&self, ty: HMIRTypeID) -> Option<MIRTypeID> {
        todo!()
    }

    pub fn cache_lowered(&mut self, ty: HMIRTypeID, mir: MIRTypeID) {
        todo!()
    }
}
