use cx_target::ArchitectureConfig;

use crate::ty::{HMIRType, HMIRTypeID};

pub trait HMIRTypeContext {
    fn architecture(&self) -> &ArchitectureConfig;

    fn resolve_type_id(&self, id: HMIRTypeID) -> &HMIRType;

    fn pointer_inner(&self, ty: &HMIRTypeID) -> Option<HMIRTypeID> {
        self.resolve_type_id(*ty).pointer_inner().copied()
    }

    fn reference_inner(&self, ty: &HMIRTypeID) -> Option<HMIRTypeID> {
        self.resolve_type_id(*ty).reference_inner().copied()
    }

    fn array_inner(&self, ty: &HMIRTypeID) -> Option<HMIRTypeID> {
        self.resolve_type_id(*ty).array_inner().copied()
    }
}