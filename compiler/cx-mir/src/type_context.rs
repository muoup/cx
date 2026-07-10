use cx_log::CXRawResult;
use cx_util::namespace::QualifiedName;

use crate::{
    architecture::ArchitectureConfig,
    layout::MIRTypeLayout,
    mir::data::{MIRFunctionSignature, MIRIntegerType, MIRType, MIRTypeId, MIRTypeKind},
};

pub trait MIRTypeContext {
    fn architecture(&self) -> &ArchitectureConfig;

    fn resolve_type_id(&self, id: MIRTypeId) -> &MIRType;

    fn try_resolve_type_id(&self, id: MIRTypeId) -> Option<&MIRType> {
        Some(self.resolve_type_id(id))
    }

    fn type_layout(&self, ty: &MIRType) -> CXRawResult<MIRTypeLayout> {
        crate::layout::layout_of(self, ty)
    }

    fn pointer_integer_type(&self) -> MIRIntegerType {
        MIRIntegerType::from_bytes(self.architecture().pointer_size() as u8)
            .expect("ArchitectureConfig guarantees a supported pointer size")
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
            .unwrap_or(ty)
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

    fn cvr_compatible(&self, type1: &MIRType, type2: &MIRType) -> bool {
        // Determines if type1 has any CVR qualifiers that type2 does not have. If so, they are not compatible.
        (type1.specifiers ^ type2.specifiers) & type1.specifiers == 0
    }
}
