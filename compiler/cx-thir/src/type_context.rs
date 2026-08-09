use cx_log::CXRawResult;
use cx_target::ArchitectureConfig;
use cx_util::namespace::QualifiedName;

use crate::{
    layout::THIRTypeLayout,
    thir::data::{THIRFnSignature, THIRIntType, THIRType, THIRTypeID, THIRTypeKind},
};

pub trait THIRTypeContext {
    fn architecture(&self) -> &ArchitectureConfig;

    fn resolve_type_id(&self, id: THIRTypeID) -> &THIRType;

    fn try_resolve_type_id(&self, id: THIRTypeID) -> Option<&THIRType> {
        Some(self.resolve_type_id(id))
    }

    fn type_layout(&self, ty: &THIRType) -> CXRawResult<THIRTypeLayout> {
        crate::layout::layout_of(self, ty)
    }

    fn pointer_integer_type(&self) -> THIRIntType {
        THIRIntType::from_bytes(self.architecture().pointer_size() as u8)
            .expect("ArchitectureConfig guarantees a supported pointer size")
    }

    fn type_id_lookup_identifier(&self, id: THIRTypeID) -> Option<&QualifiedName> {
        self.try_resolve_type_id(id)
            .and_then(|ty| ty.lookup_identifier())
    }

    fn ptr_inner(&self, ty: &THIRType) -> Option<&THIRType> {
        ty.ptr_inner().map(|id| self.resolve_type_id(id))
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
                        _type: THIRIntType::I8,
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

    fn cvr_compatible(&self, type1: &THIRType, type2: &THIRType) -> bool {
        // Determines if type1 has any CVR qualifiers that type2 does not have. If so, they are not compatible.
        (type1.specifiers ^ type2.specifiers) & type1.specifiers == 0
    }
}
