use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    body::{HMIRBody, HMIRLocalID},
    expr::HMIRExprID,
};

#[derive(Debug, Clone)]
pub struct HMIRFunction {
    body: HMIRBody,
    prototype: HMIRFnPrototype,
    def: Option<HMIRFnDefinition>,
}

#[derive(Debug, Clone)]
pub struct HMIRFnDefinition {
    params: Box<[HMIRLocalID]>,
    root: HMIRExprID,
}

#[derive(Debug, Clone)]
pub struct HMIRFnPrototype {
    symbol_name: CXIdent,
    linkage: LinkageMode,
    signature: HMIRFnSignature,
}

#[derive(Debug, Clone)]
pub struct HMIRFnSignature {
    stage: HMIRFunctionStage,
    return_type: HMIRExprID,
    params: Vec<HMIRFnParam>,
    variadic: bool,
    contract: HMIRContract,
}

#[derive(Debug, Clone)]
pub struct HMIRFnParam {
    comptime: bool,
    ty: HMIRExprID,
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

impl HMIRFunction {
    pub fn new(body: HMIRBody, prototype: HMIRFnPrototype, def: Option<HMIRFnDefinition>) -> Self {
        Self {
            body,
            prototype,
            def,
        }
    }

    pub fn body(&self) -> &HMIRBody {
        &self.body
    }

    pub fn prototype(&self) -> &HMIRFnPrototype {
        &self.prototype
    }

    pub fn signature(&self) -> &HMIRFnSignature {
        &self.prototype.signature
    }

    pub fn def(&self) -> Option<&HMIRFnDefinition> {
        self.def.as_ref()
    }
}

impl HMIRFnPrototype {
    pub fn new(
        symbol_name: CXIdent,
        linkage: LinkageMode,
        signature: HMIRFnSignature,
    ) -> Self {
        Self {
            symbol_name,
            linkage,
            signature,
        }
    }

    pub fn symbol_name(&self) -> &CXIdent {
        &self.symbol_name
    }

    pub fn linkage(&self) -> LinkageMode {
        self.linkage
    }

    pub fn signature(&self) -> &HMIRFnSignature {
        &self.signature
    }    
}

impl HMIRFnSignature {
    pub fn new(
        stage: HMIRFunctionStage,
        params: Vec<HMIRFnParam>,
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

    pub fn params(&self) -> &[HMIRFnParam] {
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

impl HMIRFnDefinition {
    pub fn new(params: Box<[HMIRLocalID]>, root: HMIRExprID) -> Self {
        Self { params, root }
    }

    pub fn params(&self) -> &[HMIRLocalID] {
        &self.params
    }

    pub fn root(&self) -> HMIRExprID {
        self.root
    }
}

impl HMIRFnParam {
    pub fn new(comptime: bool, ty: HMIRExprID) -> Self {
        Self { comptime, ty }
    }

    pub fn comptime(&self) -> bool {
        self.comptime
    }

    pub fn ty(&self) -> HMIRExprID {
        self.ty
    }
}
