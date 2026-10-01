use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{binding::HMIRLocalID, body::HMIRBody, expr::kind::HMIRExprID};

#[derive(Debug, Clone, Default)]
pub struct HMIRContract {
    safe: bool,
    precondition: Option<HMIRExprID>,
    postcondition: Option<(Option<HMIRLocalID>, HMIRExprID)>,
}

#[derive(Debug, Clone)]
pub struct HMIRSignature {
    params: Vec<HMIRLocalID>,
    return_type: HMIRExprID,
    variadic: bool,
    linkage: LinkageMode,
    link_name: CXIdent,
    contract: HMIRContract,
}

#[derive(Debug, Clone)]
pub struct HMIRFunction {
    stage: HMIRFunctionStage,
    body: HMIRBody,
    signature: HMIRSignature,
    root: Option<HMIRExprID>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRFunctionStage {
    Runtime,
    Comptime,
}

impl HMIRContract {
    pub fn new(
        safe: bool,
        precondition: Option<HMIRExprID>,
        postcondition: Option<(Option<HMIRLocalID>, HMIRExprID)>,
    ) -> Self {
        Self {
            safe,
            precondition,
            postcondition,
        }
    }

    pub fn is_safe(&self) -> bool {
        self.safe
    }

    pub fn precondition(&self) -> Option<HMIRExprID> {
        self.precondition
    }

    pub fn postcondition(&self) -> Option<(Option<HMIRLocalID>, HMIRExprID)> {
        self.postcondition
    }

    pub fn is_empty(&self) -> bool {
        !self.safe && self.precondition.is_none() && self.postcondition.is_none()
    }
}

impl HMIRSignature {
    pub fn new(
        params: Vec<HMIRLocalID>,
        return_type: HMIRExprID,
        variadic: bool,
        linkage: LinkageMode,
        link_name: CXIdent,
        contract: HMIRContract,
    ) -> Self {
        Self {
            params,
            return_type,
            variadic,
            linkage,
            link_name,
            contract,
        }
    }

    pub fn params(&self) -> &[HMIRLocalID] {
        &self.params
    }

    pub fn return_type(&self) -> HMIRExprID {
        self.return_type
    }

    pub fn is_variadic(&self) -> bool {
        self.variadic
    }

    pub fn linkage(&self) -> LinkageMode {
        self.linkage
    }

    pub fn link_name(&self) -> &CXIdent {
        &self.link_name
    }

    pub fn contract(&self) -> &HMIRContract {
        &self.contract
    }
}

impl HMIRFunction {
    pub fn new(
        stage: HMIRFunctionStage,
        body: HMIRBody,
        signature: HMIRSignature,
        root: Option<HMIRExprID>,
    ) -> Self {
        Self {
            stage,
            body,
            signature,
            root,
        }
    }

    pub fn stage(&self) -> HMIRFunctionStage {
        self.stage
    }

    pub fn body(&self) -> &HMIRBody {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut HMIRBody {
        &mut self.body
    }

    pub fn signature(&self) -> &HMIRSignature {
        &self.signature
    }

    pub fn root(&self) -> Option<HMIRExprID> {
        self.root
    }

    pub fn has_comptime_params(&self) -> bool {
        self.signature
            .params
            .iter()
            .any(|param| self.body.local(*param).is_comptime())
    }
}
