use crate::mir::data::{MIRIntegerType, MIRType, MIRTypeId, MIRTypeKind};

pub trait MIRTypeContext {
    fn resolve_type_id(&self, id: MIRTypeId) -> &MIRType;

    fn ptr_inner(&self, ty: &MIRType) -> Option<&MIRType> {
        ty.ptr_inner()
            .map(|id| self.resolve_type_id(id))
    }

    fn mem_ref_inner(&self, ty: &MIRType) -> Option<&MIRType> {
        ty.mem_ref_inner()
            .map(|id| self.resolve_type_id(id))
    }

    fn array_inner(&self, ty: &MIRType) -> Option<&MIRType> {
        ty.array_inner()
            .map(|id| self.resolve_type_id(id))
    }

    fn is_c_str(&self, ty: &MIRType) -> bool {
        self.ptr_inner(ty)
            .map(|ty| matches!(ty.kind, MIRTypeKind::Integer { _type: MIRIntegerType::I8, signed: false }))
            .unwrap_or(false)
    }

    fn is_cx_str(&self, ty: &MIRType) -> bool {
        self.ptr_inner(ty)
            .map(|ty| ty.is_str())
            .unwrap_or(false)
    }
}
