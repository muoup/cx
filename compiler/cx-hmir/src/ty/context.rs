use cx_target::ArchitectureConfig;

use crate::ty::{HMIRType, intrinsic::{HMIRIntType, HMIRTypeID}};

pub trait HMIRTypeContext {
    fn architecture(&self) -> &ArchitectureConfig;

    fn resolve_type_id(&self, id: HMIRTypeID) -> Option<&HMIRType>;

    fn ptr_inner(&self, ty: &HMIRTypeID) -> Option<&HMIRTypeID> {
        self.resolve_type_id(*ty).and_then(|ty| ty.ptr_inner())
    }

    fn mem_ref_inner(&self, ty: &THIRType) -> Option<&THIRType> {
        ty.mem_ref_inner().map(|id| self.resolve_type_id(id))
    }

    fn array_inner(&self, ty: &THIRType) -> Option<&THIRType> {
        ty.array_inner().map(|id| self.resolve_type_id(id))
    }

    fn intern_signature<'a>(&'a self, ty: &'a THIRType) -> Option<&'a THIRFnSignature> {
        if let THIRTypeKind::Function { signature } = &self
            .ptr_inner(ty)
            .or_else(|| self.mem_ref_inner(ty))
            .unwrap_or(ty)
            .kind
        {
            return Some(signature.as_ref());
        }

        None
    }

    fn is_c_str(&self, ty: &THIRType) -> bool {
        self.ptr_inner(ty)
            .map(|ty| {
                matches!(
                    ty.kind,
                    THIRTypeKind::Integer {
                        ty: THIRIntType::I8,
                        signed: false
                    }
                )
            })
            .unwrap_or(false)
    }

    fn is_cx_str(&self, ty: &THIRType) -> bool {
        self.mem_ref_inner(ty)
            .map(|ty| ty.is_str())
            .unwrap_or(false)
    }



    fn type_debug_name(&self, ty: &THIRType) -> Option<String>
    where
        Self: Sized,
    {
        ty.strong_identifier()
            .map(|_| ty.display_with(self).to_string())
    }
}
