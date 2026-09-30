use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::expr::meta::HMIRMetaID;

dense_id!(HMIRMetaLocalID, "$");
dense_id!(HMIRObjLocalID, "%");
dense_id!(HMIRHoleID, "?");

#[derive(Debug, Clone)]
pub struct HMIRLocal {
    name: Option<CXIdent>,
    ty: HMIRMetaID,
    span: TokenRange,
}

#[derive(Debug, Clone)]
pub struct HMIRHole {
    span: TokenRange,
}

impl HMIRLocal {
    pub fn new(name: Option<CXIdent>, ty: HMIRMetaID, span: TokenRange) -> Self {
        Self { name, ty, span }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn ty(&self) -> HMIRMetaID {
        self.ty
    }

    pub fn set_ty(&mut self, ty: HMIRMetaID) {
        self.ty = ty;
    }

    pub fn span(&self) -> &TokenRange {
        &self.span
    }
}

impl HMIRHole {
    pub fn new(span: TokenRange) -> Self {
        Self { span }
    }

    pub fn span(&self) -> &TokenRange {
        &self.span
    }
}
