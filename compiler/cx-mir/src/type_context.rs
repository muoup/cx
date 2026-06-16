use cx_util::namespace::QualifiedName;

use crate::mir::data::{MIRFunctionSignature, MIRIntegerType, MIRType, MIRTypeId, MIRTypeKind};

pub trait MIRTypeContext {
    fn resolve_type_id(&self, id: MIRTypeId) -> &MIRType;

    fn try_resolve_type_id(&self, id: MIRTypeId) -> Option<&MIRType> {
        Some(self.resolve_type_id(id))
    }

    fn type_id_lookup_identifier(&self, id: MIRTypeId) -> Option<&QualifiedName> {
        self.try_resolve_type_id(id)
            .and_then(|ty| ty.lookup_identifier())
    }

    fn ptr_inner(&self, ty: &MIRType) -> Option<&MIRType> {
        ty.ptr_inner().map(|id| self.resolve_type_id(id))
    }

    fn mem_ref_inner(&self, ty: &MIRType) -> Option<&MIRType> {
        ty.mem_ref_inner().map(|id| self.resolve_type_id(id))
    }

    fn array_inner(&self, ty: &MIRType) -> Option<&MIRType> {
        ty.array_inner().map(|id| self.resolve_type_id(id))
    }

    fn intern_signature<'a>(&'a self, ty: &'a MIRType) -> Option<&'a MIRFunctionSignature> {
        if let MIRTypeKind::Function { signature } = &self
            .ptr_inner(ty)
            .or_else(|| self.mem_ref_inner(ty))
            .unwrap_or_else(|| ty)
            .kind
        {
            return Some(signature.as_ref());
        }

        None
    }

    fn is_c_str(&self, ty: &MIRType) -> bool {
        self.ptr_inner(ty)
            .map(|ty| {
                matches!(
                    ty.kind,
                    MIRTypeKind::Integer {
                        _type: MIRIntegerType::I8,
                        signed: false
                    }
                )
            })
            .unwrap_or(false)
    }

    fn is_cx_str(&self, ty: &MIRType) -> bool {
        self.mem_ref_inner(ty)
            .map(|ty| ty.is_str())
            .unwrap_or(false)
    }
}
