use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{binding::HMIRLocalID, body::HMIRBody, expr::HMIRExprID, ty::HMIRTypeID};

#[derive(Debug, Clone)]
pub struct HMIRFunction {
    body: HMIRBody,

    symbol_name: CXIdent,
    signature: HMIRFnSignature,
    linkage: LinkageMode,

    def: Option<HMIRFunctionDef>,
}

#[derive(Debug, Clone)]
pub struct HMIRFunctionDef {
    params: Box<[HMIRTypeID]>,
    root: HMIRExprID,
}

#[derive(Debug, Clone)]
pub struct HMIRFnSignature {
    stage: HMIRFunctionStage,
    return_type: HMIRExprID,
    params: Vec<HMIRTypeID>,
    variadic: bool,
    contract: HMIRContract,
}

#[derive(Debug, Clone, Default)]
pub struct HMIRContract {
    safe: bool,
    precondition: Option<HMIRExprID>,
    postcondition: Option<(Option<HMIRLocalID>, HMIRExprID)>,
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

impl HMIRFnSignature {
    pub fn new(
        stage: HMIRFunctionStage,
        params: Vec<HMIRTypeID>,
        return_type: HMIRExprID,
        variadic: bool,
        contract: HMIRContract,
    ) -> Self {
        Self {
            stage,
            params,
            return_type,
            variadic,
            contract,
        }
    }

    pub fn stage(&self) -> HMIRFunctionStage {
        self.stage
    }

    pub fn params(&self) -> &[HMIRTypeID] {
        &self.params
    }

    pub fn return_type(&self) -> HMIRExprID {
        self.return_type
    }

    pub fn is_variadic(&self) -> bool {
        self.variadic
    }

    pub fn contract(&self) -> &HMIRContract {
        &self.contract
    }
}

impl HMIRFunction {
    pub fn new(
        symbol_name: CXIdent,
        linkage: LinkageMode,
        body: HMIRBody,
        signature: HMIRFnSignature,
        def: Option<HMIRFunctionDef>,
    ) -> Self {
        Self {
            symbol_name,
            linkage,
            body,
            signature,
            def,
        }
    }

    pub fn body(&self) -> &HMIRBody {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut HMIRBody {
        &mut self.body
    }

    pub fn signature(&self) -> &HMIRFnSignature {
        &self.signature
    }

    pub fn def(&self) -> Option<&HMIRFunctionDef> {
        self.def.as_ref()
    }
}
