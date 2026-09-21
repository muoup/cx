use std::collections::HashSet;

use cx_target::ArchitectureConfig;

use crate::ty::{MIRType, MIRTypeID, MIRTypeKind, comparison::same_type_inner};

pub trait MTRegistry: Sized {
    // --- INTERFACE ---

    fn architecture(&self) -> &ArchitectureConfig;
    fn definition(&self, id: MIRTypeID) -> Option<&MIRType>;
    fn find(&self, ty: &MIRType) -> Option<MIRTypeID>;
    fn find_kind(&self, kind: &MIRTypeKind) -> Option<MIRTypeID>;
    fn debug_name(&self, id: MIRTypeID) -> Option<&str>;

    // --- HELPERS ---

    fn unit(&self) -> MIRTypeID {
        MIRTypeID::new(0)
    }

    fn same_type(&self, left: MIRTypeID, right: MIRTypeID) -> bool {
        same_type_inner(self, &mut HashSet::new(), left, right)
    }

    fn is_reference_type(&self, ty: &MIRType) -> bool {
        matches!(ty.kind, MIRTypeKind::MemoryReference { .. })
    }

    fn reference_inner(&self, ty: &MIRType) -> Option<MIRTypeID> {
        match &ty.kind {
            MIRTypeKind::MemoryReference { inner, .. } => Some(*inner),
            _ => None,
        }
    }

    fn is_pointer_type(&self, ty: &MIRType) -> bool {
        matches!(ty.kind, MIRTypeKind::PointerTo { .. })
    }

    fn pointer_inner(&self, ty: &MIRType) -> Option<MIRTypeID> {
        match &ty.kind {
            MIRTypeKind::PointerTo { inner } => Some(*inner),
            _ => None,
        }
    }
}
