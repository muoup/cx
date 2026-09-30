use cx_util::linkage::LinkageMode;

use crate::{body::HMIRBody, expr::kind::HMIRExprID};

#[derive(Debug, Clone)]
pub struct HMIRGlobal {
    body: HMIRBody,
    ty: HMIRExprID,
    initializer: Option<HMIRExprID>,
    mutable: bool,
    linkage: LinkageMode,
}

impl HMIRGlobal {
    pub fn new(
        body: HMIRBody,
        ty: HMIRExprID,
        initializer: Option<HMIRExprID>,
        mutable: bool,
        linkage: LinkageMode,
    ) -> Self {
        Self {
            body,
            ty,
            initializer,
            mutable,
            linkage,
        }
    }

    pub fn body(&self) -> &HMIRBody {
        &self.body
    }

    pub fn ty(&self) -> HMIRExprID {
        self.ty
    }

    pub fn initializer(&self) -> Option<HMIRExprID> {
        self.initializer
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn linkage(&self) -> LinkageMode {
        self.linkage
    }
}
