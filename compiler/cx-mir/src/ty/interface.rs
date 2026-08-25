use std::collections::HashSet;

use cx_target::ArchitectureConfig;

use crate::{
    MIRIntType, MIRLayoutError, MIRType, MIRTypeID, MIRTypeKind, MIRTypeLayout,
    ty::comparison::same_type_inner,
};

pub trait MTRegistry: Sized {
    fn architecture(&self) -> &ArchitectureConfig;
    fn definition(&self, id: MIRTypeID) -> Option<&MIRType>;
    fn find(&self, ty: &MIRType) -> Option<MIRTypeID>;
    fn debug_name(&self, id: MIRTypeID) -> Option<&str>;

    fn unit(&self) -> MIRTypeID {
        MIRTypeID::new(0)
    }

    fn resolve_type_id(&self, id: MIRTypeID) -> Result<&MIRType, MIRLayoutError> {
        self.definition(id).ok_or(MIRLayoutError::InvalidType(id))
    }

    fn kind(&self, id: MIRTypeID) -> Result<&MIRTypeKind, MIRLayoutError> {
        self.resolve_type_id(id).and_then(|ty| Ok(&ty.kind))
    }

    fn layout(&self, id: MIRTypeID) -> Result<Option<&MIRTypeLayout>, MIRLayoutError> {
        self.resolve_type_id(id)
            .map(|ty| ty.layout.as_ref())
    }

    fn pointer_integer_type(&self) -> MIRIntType {
        MIRIntType::from_bytes(self.architecture().pointer_size() as u8)
            .expect("ArchitectureConfig guarantees a supported pointer size")
    }

    fn same_type(&self, left: MIRTypeID, right: MIRTypeID) -> bool {
        same_type_inner(self, &mut HashSet::new(), left, right)
    }

    fn is_reference_type(&self, id: MIRTypeID) -> Result<bool, MIRLayoutError> {
        let ty = self.resolve_type_id(id)?;
        Ok(matches!(ty.kind, MIRTypeKind::MemoryReference { .. }))
    }

    fn reference_inner(&self, id: MIRTypeID) -> Result<Option<MIRTypeID>, MIRLayoutError> {
        let ty = self.resolve_type_id(id)?;
        match &ty.kind {
            MIRTypeKind::MemoryReference { inner, .. } => Ok(Some(*inner)),
            _ => Ok(None),
        }
    }

    fn is_pointer_type(&self, id: MIRTypeID) -> Result<bool, MIRLayoutError> {
        let ty = self.resolve_type_id(id)?;
        Ok(matches!(ty.kind, MIRTypeKind::PointerTo { .. }))
    }

    fn pointer_inner(&self, id: MIRTypeID) -> Result<Option<MIRTypeID>, MIRLayoutError> {
        let ty = self.resolve_type_id(id)?;
        match &ty.kind {
            MIRTypeKind::PointerTo { inner } => Ok(Some(*inner)),
            _ => Ok(None),
        }
    }
}
