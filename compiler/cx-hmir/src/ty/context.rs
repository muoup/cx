use cx_target::ArchitectureConfig;

use crate::ty::{HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics};

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

    fn pointer_inner(&self, ty: HMIRTypeID) -> Option<HMIRTypeID> {
        self.resolve_type_id(self.underlying(ty))
            .pointer_inner()
            .copied()
    }

    fn is_pointer(&self, ty: HMIRTypeID) -> bool {
        matches!(self.resolve_kind(ty), HMIRTypeKind::PointerTo(_))
    }

    fn reference_inner(&self, ty: HMIRTypeID) -> Option<HMIRTypeID> {
        self.resolve_type_id(self.underlying(ty))
            .reference_inner()
            .copied()
    }

    fn is_reference(&self, ty: HMIRTypeID) -> bool {
        matches!(self.resolve_kind(ty), HMIRTypeKind::ReferenceTo(_))
    }

    fn array_inner(&self, ty: HMIRTypeID) -> Option<HMIRTypeID> {
        self.resolve_type_id(self.underlying(ty))
            .array_inner()
            .copied()
    }

    fn is_array(&self, ty: HMIRTypeID) -> bool {
        matches!(self.resolve_kind(ty), HMIRTypeKind::Array { .. })
    }

    fn is_float(&self, ty: HMIRTypeID) -> bool {
        matches!(self.resolve_kind(ty), HMIRTypeKind::Float(_))
    }

    fn is_int(&self, ty: HMIRTypeID) -> bool {
        matches!(self.resolve_kind(ty), HMIRTypeKind::Int(_))
    }

    // The qualifiers of 'ty' itself; those of a type it references or points to are not its own
    fn semantics(&self, ty: HMIRTypeID) -> HMIRTypeSemantics {
        let resolved = self.resolve_type_id(ty);
        match resolved.kind() {
            HMIRTypeKind::Nominal(inner) => resolved.semantics().apply(self.semantics(*inner)),
            _ => *resolved.semantics(),
        }
    }

    fn is_nodrop(&self, ty: HMIRTypeID) -> bool {
        self.semantics(ty).move_semantics.is_nodrop()
    }

    fn is_nocopy(&self, ty: HMIRTypeID) -> bool {
        self.semantics(ty).move_semantics.is_nocopy()
    }
}