use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{body::HMIRBody, expr::HMIRExprID};

#[derive(Debug, Clone)]
pub struct HMIRGlobal {
    body: HMIRBody,
    
    link_name: CXIdent,
    linkage: LinkageMode,

    ty: HMIRExprID,
    initializer: Option<HMIRExprID>,
}

// Evaluated entirely by the stager and never emitted (enum variants, type definitions)
#[derive(Debug, Clone)]
pub struct HMIRComptimeGlobal {
    body: HMIRBody,
    ty: HMIRExprID,
    initializer: HMIRExprID,
}

impl HMIRGlobal {
    pub fn new(
        body: HMIRBody,
        ty: HMIRExprID,
        initializer: Option<HMIRExprID>,
        linkage: LinkageMode,
        link_name: CXIdent,
    ) -> Self {
        Self {
            body,
            ty,
            initializer,
            linkage,
            link_name,
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

    pub fn linkage(&self) -> LinkageMode {
        self.linkage
    }

    pub fn link_name(&self) -> &CXIdent {
        &self.link_name
    }
}

impl HMIRComptimeGlobal {
    pub fn new(body: HMIRBody, ty: HMIRExprID, initializer: HMIRExprID) -> Self {
        Self {
            body,
            ty,
            initializer,
        }
    }

    pub fn body(&self) -> &HMIRBody {
        &self.body
    }

    pub fn ty(&self) -> HMIRExprID {
        self.ty
    }

    pub fn initializer(&self) -> HMIRExprID {
        self.initializer
    }
}
