use cx_util::linkage::LinkageMode;

use crate::{
    binding::{HMIRMetaLocalID, HMIRObjLocalID},
    body::HMIRBody,
    expr::{meta::HMIRMetaID, obj::HMIRObjID},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HMIRParam {
    Static(HMIRMetaLocalID),
    Runtime(HMIRObjLocalID),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HMIRFunctionRoot {
    Meta(HMIRMetaID),
    Obj(HMIRObjID),
}

#[derive(Debug, Clone, Default)]
pub struct HMIRContract {
    safe: bool,
    precondition: Option<HMIRObjID>,
    postcondition: Option<(Option<HMIRObjLocalID>, HMIRObjID)>,
}

#[derive(Debug, Clone)]
pub struct HMIRSignature {
    params: Vec<HMIRParam>,
    return_type: HMIRMetaID,
    variadic: bool,
    linkage: LinkageMode,
    contract: HMIRContract,
}

#[derive(Debug, Clone)]
pub struct HMIRFunction {
    body: HMIRBody,
    signature: HMIRSignature,
    root: Option<HMIRFunctionRoot>,
}

impl HMIRContract {
    pub fn new(
        safe: bool,
        precondition: Option<HMIRObjID>,
        postcondition: Option<(Option<HMIRObjLocalID>, HMIRObjID)>,
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

    pub fn precondition(&self) -> Option<HMIRObjID> {
        self.precondition
    }

    pub fn postcondition(&self) -> Option<(Option<HMIRObjLocalID>, HMIRObjID)> {
        self.postcondition
    }

    pub fn is_empty(&self) -> bool {
        !self.safe && self.precondition.is_none() && self.postcondition.is_none()
    }
}

impl HMIRSignature {
    pub fn new(
        params: Vec<HMIRParam>,
        return_type: HMIRMetaID,
        variadic: bool,
        linkage: LinkageMode,
        contract: HMIRContract,
    ) -> Self {
        Self {
            params,
            return_type,
            variadic,
            linkage,
            contract,
        }
    }

    pub fn params(&self) -> &[HMIRParam] {
        &self.params
    }

    pub fn return_type(&self) -> HMIRMetaID {
        self.return_type
    }

    pub fn is_variadic(&self) -> bool {
        self.variadic
    }

    pub fn linkage(&self) -> LinkageMode {
        self.linkage
    }

    pub fn contract(&self) -> &HMIRContract {
        &self.contract
    }
}

impl HMIRFunction {
    pub fn new(body: HMIRBody, signature: HMIRSignature, root: Option<HMIRFunctionRoot>) -> Self {
        Self {
            body,
            signature,
            root,
        }
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

    pub fn root(&self) -> Option<HMIRFunctionRoot> {
        self.root
    }

    pub fn is_static(&self) -> bool {
        self.signature
            .params
            .iter()
            .any(|param| matches!(param, HMIRParam::Static(_)))
    }
}
