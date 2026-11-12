use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    body::{HMIRBodyContext, HMIRLocalID},
    expr::HMIRExprID,
};

#[derive(Debug, Clone)]
pub struct HMIRFunction {
    body: HMIRBodyContext,
    prototype: HMIRFnPrototype,
    params: Box<[HMIRLocalID]>,
    def: Option<HMIRFnDefinition>,
}

#[derive(Debug, Clone)]
pub struct HMIRFnDefinition {
    root: HMIRExprID,
    address_labels: Vec<CXIdent>,
}

#[derive(Debug, Clone)]
pub struct HMIRFnPrototype {
    symbol_name: CXIdent,
    linkage: LinkageMode,
    signature: HMIRFnSignature<HMIRExprID>,
    params: Vec<HMIRFnParamDecl>,
    defaults: HMIRBodyContext,
}

// What a declaration says of a parameter beyond its type, which a function type does not carry: only a call that
// names the function can bind an argument by name or leave a default to stand. A default is an expression of the
// prototype's own body, which declares no locals
#[derive(Debug, Clone)]
pub struct HMIRFnParamDecl {
    name: Option<CXIdent>,
    default: Option<HMIRExprID>,
}

#[derive(Debug, Clone)]
pub struct HMIRFnSignature<T> {
    stage: HMIRFunctionStage,
    return_type: T,
    params: Vec<HMIRFnParam<T>>,
    variadic: bool,
    contract: HMIRContract,
}

#[derive(Debug, Clone)]
pub struct HMIRFnParam<T> {
    comptime: bool,
    ty: T,
}

// The conditions are expressions of the contract's own body, which binds the parameters they name by position
#[derive(Debug, Clone)]
pub struct HMIRContract {
    body: HMIRBodyContext,
    params: Box<[HMIRLocalID]>,

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
    pub fn new(
        body: HMIRBodyContext,
        prototype: HMIRFnPrototype,
        params: Box<[HMIRLocalID]>,
        def: Option<HMIRFnDefinition>,
    ) -> Self {
        Self {
            body,
            prototype,
            params,
            def,
        }
    }

    pub fn body(&self) -> &HMIRBodyContext {
        &self.body
    }

    pub fn params(&self) -> &[HMIRLocalID] {
        &self.params
    }

    pub fn prototype(&self) -> &HMIRFnPrototype {
        &self.prototype
    }

    pub fn signature(&self) -> &HMIRFnSignature<HMIRExprID> {
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
        signature: HMIRFnSignature<HMIRExprID>,
        params: Vec<HMIRFnParamDecl>,
        defaults: HMIRBodyContext,
    ) -> Self {
        Self {
            symbol_name,
            linkage,
            signature,
            params,
            defaults,
        }
    }

    pub fn defaults(&self) -> &HMIRBodyContext {
        &self.defaults
    }

    pub fn symbol_name(&self) -> &CXIdent {
        &self.symbol_name
    }

    pub fn linkage(&self) -> LinkageMode {
        self.linkage
    }

    pub fn signature(&self) -> &HMIRFnSignature<HMIRExprID> {
        &self.signature
    }

    pub fn params(&self) -> &[HMIRFnParamDecl] {
        &self.params
    }
}

impl HMIRFnParamDecl {
    pub fn new(name: Option<CXIdent>, default: Option<HMIRExprID>) -> Self {
        Self { name, default }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    // Only a named argument binds a parameter with a default; left unbound, it takes the default
    pub fn default(&self) -> Option<HMIRExprID> {
        self.default
    }
}

impl<T> HMIRFnSignature<T> {
    pub fn new(
        stage: HMIRFunctionStage,
        params: Vec<HMIRFnParam<T>>,
        return_type: T,
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

    pub fn params(&self) -> &[HMIRFnParam<T>] {
        &self.params
    }

    pub fn is_comptime_param(&self, index: usize) -> bool {
        self.stage == HMIRFunctionStage::Comptime || self.params[index].comptime
    }

    pub fn return_type(&self) -> &T {
        &self.return_type
    }

    pub fn is_variadic(&self) -> bool {
        self.variadic
    }

    pub fn contract(&self) -> &HMIRContract {
        &self.contract
    }

    pub fn map<U, E>(
        &self,
        mut ty: impl FnMut(&T) -> Result<U, E>,
    ) -> Result<HMIRFnSignature<U>, E> {
        let params = self
            .params
            .iter()
            .map(|param| Ok(HMIRFnParam::new(param.comptime, ty(&param.ty)?)))
            .collect::<Result<_, E>>()?;

        Ok(HMIRFnSignature {
            stage: self.stage,
            params,
            return_type: ty(&self.return_type)?,
            variadic: self.variadic,
            contract: self.contract.clone(),
        })
    }
}

impl HMIRContract {
    pub fn new(
        body: HMIRBodyContext,
        params: Box<[HMIRLocalID]>,
        safe: bool,
        precondition: Option<HMIRExprID>,
        postcondition: Option<(Option<HMIRLocalID>, HMIRExprID)>,
    ) -> Self {
        Self {
            body,
            params,
            safe,
            precondition,
            postcondition,
        }
    }

    pub fn body(&self) -> &HMIRBodyContext {
        &self.body
    }

    pub fn params(&self) -> &[HMIRLocalID] {
        &self.params
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

impl Default for HMIRContract {
    fn default() -> Self {
        Self::new(HMIRBodyContext::new(), Box::default(), false, None, None)
    }
}

impl HMIRFnDefinition {
    pub fn new(root: HMIRExprID, address_labels: Vec<CXIdent>) -> Self {
        Self {
            root,
            address_labels,
        }
    }

    pub fn root(&self) -> HMIRExprID {
        self.root
    }

    pub fn address_labels(&self) -> &[CXIdent] {
        &self.address_labels
    }
}

impl<T> HMIRFnParam<T> {
    pub fn new(comptime: bool, ty: T) -> Self {
        Self { comptime, ty }
    }

    pub fn comptime(&self) -> bool {
        self.comptime
    }

    pub fn ty(&self) -> &T {
        &self.ty
    }
}
