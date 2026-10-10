use std::collections::HashMap;

use cx_hmir::{HMIRFnSignature, body::HMIRLocalID};
use cx_log::CXResult;
use cx_mir::{MIRFunction, MIRScopeID, MIRValue};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::MIRFunctionBuilder},
    lower::{HMIRValue, HMIRValueKind, constant::mir_constant},
};

#[derive(Debug)]
pub struct FnLoweringContext<'env, 'global, 'hmir> {
    env: &'env mut HMIREnvironment<'global, 'hmir>,
    signature: &'hmir HMIRFnSignature,
    builder: MIRFunctionBuilder,

    locals: HashMap<HMIRLocalID, HMIRValue>,
    open_scopes: Vec<MIRScopeID>,
}

impl<'env, 'global, 'hmir> FnLoweringContext<'env, 'global, 'hmir> {
    pub fn new(env: &'env mut HMIREnvironment<'global, 'hmir>, signature: &'hmir HMIRFnSignature) -> Self {
        let mir_prototype = todo!("Lower prototype");

        Self {
            env,
            signature,
            builder: MIRFunctionBuilder::new(mir_prototype),

            locals: HashMap::new(),
            open_scopes: Vec::new(),
        }
    }

    pub fn env(&self) -> &HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn env_mut(&mut self) -> &mut HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn signature(&self) -> &'hmir HMIRFnSignature {
        self.signature
    }

    pub fn builder(&self) -> &MIRFunctionBuilder {
        &self.builder
    }

    pub fn builder_mut(&mut self) -> &mut MIRFunctionBuilder {
        &mut self.builder
    }

    pub fn insert_local(&mut self, id: HMIRLocalID, value: HMIRValue) {
        self.locals.insert(id, value);
    }

    pub fn get_local(&self, id: &HMIRLocalID) -> Option<&HMIRValue> {
        self.locals.get(id)
    }

    pub fn open_scopes(&self) -> &[MIRScopeID] {
        &self.open_scopes
    }

    pub fn push_scope(&mut self, scope: MIRScopeID) {
        self.open_scopes.push(scope);
    }

    pub fn pop_scope(&mut self) -> MIRScopeID {
        self.open_scopes
            .pop()
            .expect("Attempt to pop a scope when none is open")
    }

    pub fn materialize(&mut self, value: &HMIRValue, span: &TokenRange) -> CXResult<MIRValue> {
        Ok(match value.kind() {
            HMIRValueKind::Constant(constant) => {
                MIRValue::Constant(mir_constant(self.env, constant, span)?)
            }
            HMIRValueKind::Register(register) => MIRValue::Register(*register),
            HMIRValueKind::Place(place) => MIRValue::PlaceRef(*place),
        })
    }

    pub fn finish(self) -> MIRFunction {
        self.builder.finish()
    }
}
