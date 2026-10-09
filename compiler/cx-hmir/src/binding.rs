use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::expr::HMIRExprID;

dense_id!(HMIRLocalID, "%");
dense_id!(HMIRHoleID, "?");

#[derive(Debug, Clone)]
pub struct HMIRLocal {
    name: Option<CXIdent>,
    ty: HMIRExprID,
    comptime: bool,
    span: TokenRange,
}

#[derive(Debug, Clone)]
pub struct HMIRHole {
    span: TokenRange,
}

impl HMIRLocal {
    pub fn new(name: Option<CXIdent>, ty: HMIRExprID, comptime: bool, span: TokenRange) -> Self {
        Self {
            name,
            ty,
            comptime,
            span,
        }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn ty(&self) -> HMIRExprID {
        self.ty
    }

    pub fn set_ty(&mut self, ty: HMIRExprID) {
        self.ty = ty;
    }

    pub fn is_comptime(&self) -> bool {
        self.comptime
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
