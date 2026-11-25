use chl_util::{identifier::CHLIdent, linkage::LinkageMode};

use crate::{body::HMIRBodyContext, expr::HMIRExprID};

#[derive(Debug, Clone)]
pub struct HMIRGlobal {
    body: HMIRBodyContext,
    
    symbol_name: CHLIdent,
    linkage: LinkageMode,
    explicit_extern: bool,

    ty: HMIRExprID,
    initializer: Option<HMIRExprID>,
}

// Evaluated entirely by the stager and never emitted (enum variants, type definitions)
#[derive(Debug, Clone)]
pub struct HMIRComptimeGlobal {
    body: HMIRBodyContext,
    ty: HMIRExprID,
    initializer: HMIRExprID,
}

impl HMIRGlobal {
    pub fn new(
        body: HMIRBodyContext,
        ty: HMIRExprID,
        initializer: Option<HMIRExprID>,
        linkage: LinkageMode,
        symbol_name: CHLIdent,
        explicit_extern: bool,
    ) -> Self {
        Self {
            body,
            ty,
            initializer,
            linkage,
            symbol_name,
            explicit_extern,
        }
    }

    pub fn body(&self) -> &HMIRBodyContext {
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

    pub fn is_explicit_extern(&self) -> bool {
        self.explicit_extern
    }

    pub fn symbol_name(&self) -> &CHLIdent {
        &self.symbol_name
    }
}

impl HMIRComptimeGlobal {
    pub fn new(body: HMIRBodyContext, ty: HMIRExprID, initializer: HMIRExprID) -> Self {
        Self {
            body,
            ty,
            initializer,
        }
    }

    pub fn body(&self) -> &HMIRBodyContext {
        &self.body
    }

    pub fn ty(&self) -> HMIRExprID {
        self.ty
    }

    pub fn initializer(&self) -> HMIRExprID {
        self.initializer
    }

    pub fn declares_type(&self) -> bool {
        self.body.is_type_kind(self.ty)
    }
}
