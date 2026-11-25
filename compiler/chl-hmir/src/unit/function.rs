use chl_util::{identifier::CHLIdent, linkage::LinkageMode};

use crate::{
    body::{HMIRBodyContext, HMIRHole, HMIRLocal, HMIRLocalID},
    expr::{HMIRExpr, HMIRExprID, HMIRExprKind},
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
    address_labels: Vec<CHLIdent>,
    transparent: bool,
}

#[derive(Debug, Clone)]
pub struct HMIRFnPrototype {
    symbol_name: CHLIdent,
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
    name: Option<CHLIdent>,
    default: Option<HMIRExprID>,
    inferred: bool,
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

    pub fn body_mut(&mut self) -> &mut HMIRBodyContext {
        &mut self.body
    }

    pub fn prepend_inferred_parameters(&mut self, locals: &[HMIRLocalID]) {
        if locals.is_empty() {
            return;
        }
        let prototype = &mut self.prototype;
        let mut declarations = Vec::new();
        let mut parameters = Vec::new();
        let mut contract_locals = Vec::new();
        for &local in locals {
            let binding = self.body.local(local);
            let span = binding.span();
            let default = prototype.defaults.declare_hole(HMIRHole::new(span.clone()));
            let default = prototype
                .defaults
                .push_expr(HMIRExpr::new(HMIRExprKind::Hole(default), span.clone()));
            declarations.push(HMIRFnParamDecl {
                name: None,
                default: Some(default),
                inferred: true,
            });
            parameters.push(HMIRFnParam::new(true, binding.ty()));
            if binding.name().is_none() && !prototype.signature.contract.params.is_empty() {
                let contract = &mut prototype.signature.contract.body;
                let hole = contract.declare_hole(HMIRHole::new(span.clone()));
                let ty = contract.push_expr(HMIRExpr::new(HMIRExprKind::Hole(hole), span.clone()));
                contract_locals.push(contract.declare_local(HMIRLocal::new(
                    None,
                    ty,
                    span.clone(),
                    true,
                )));
            }
        }
        prototype.params.splice(0..0, declarations);
        prototype.signature.params.splice(0..0, parameters);
        let contract = &mut prototype.signature.contract;
        if !contract_locals.is_empty() {
            contract_locals.extend_from_slice(&contract.params);
            contract.params = contract_locals.into();
        }
        self.params = locals.iter().chain(self.params.iter()).copied().collect();
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

    pub fn structural_body(&self) -> Option<HMIRExprID> {
        let root = self.transparent_body()?;
        let mut operand = root;
        while let crate::expr::HMIRExprKind::Type(ty) = self.body().expr(operand).kind() {
            match ty.kind() {
                crate::ty::HMIRExprTypeKind::Nominal { inner, strong_identifier: None } => {
                    operand = *inner;
                }
                crate::ty::HMIRExprTypeKind::Strong { .. } => return None,
                _ => break,
            }
        }
        Some(root)
    }

    pub fn transparent_body(&self) -> Option<HMIRExprID> {
        self.def()
            .filter(|def| def.transparent)
            .map(HMIRFnDefinition::root)
    }

    pub fn returns_type(&self) -> bool {
        self.body.is_type_kind(*self.signature().return_type())
    }
}

impl HMIRFnPrototype {
    pub fn new(
        symbol_name: CHLIdent,
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

    pub fn symbol_name(&self) -> &CHLIdent {
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
    pub fn new(name: Option<CHLIdent>, default: Option<HMIRExprID>) -> Self {
        Self {
            name,
            default,
            inferred: false,
        }
    }

    pub fn name(&self) -> Option<&CHLIdent> {
        self.name.as_ref()
    }

    // Only a named argument binds a parameter with a default; left unbound, it takes the default
    pub fn default(&self) -> Option<HMIRExprID> {
        self.default
    }

    pub fn inferred(&self) -> bool {
        self.inferred
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
    pub fn new(root: HMIRExprID, address_labels: Vec<CHLIdent>, transparent: bool) -> Self {
        Self {
            root,
            address_labels,
            transparent,
        }
    }

    pub fn root(&self) -> HMIRExprID {
        self.root
    }

    pub fn address_labels(&self) -> &[CHLIdent] {
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
