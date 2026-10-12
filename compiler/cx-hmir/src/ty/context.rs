use cx_target::ArchitectureConfig;

use crate::ty::{HMIRType, HMIRTypeID, HMIRTypeKind};

pub trait HMIRTypeContext {
    fn architecture(&self) -> &ArchitectureConfig;

    fn resolve_type_id(&self, id: HMIRTypeID) -> &HMIRType;

    fn underlying(&self, id: HMIRTypeID) -> HMIRTypeID {
        match self.resolve_type_id(id).kind() {
            HMIRTypeKind::Nominal(inner) => self.underlying(*inner),
            _ => id,
        }
    }

    fn resolve_kind(&self, id: HMIRTypeID) -> &HMIRTypeKind {
        self.resolve_type_id(self.underlying(id)).kind()
    }

    fn pointer_inner(&self, ty: &HMIRTypeID) -> Option<HMIRTypeID> {
        self.resolve_type_id(self.underlying(*ty))
            .pointer_inner()
            .copied()
    }

    fn reference_inner(&self, ty: &HMIRTypeID) -> Option<HMIRTypeID> {
        self.resolve_type_id(self.underlying(*ty))
            .reference_inner()
            .copied()
    }

    fn array_inner(&self, ty: &HMIRTypeID) -> Option<HMIRTypeID> {
        self.resolve_type_id(self.underlying(*ty))
            .array_inner()
            .copied()
    }
}