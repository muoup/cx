use std::collections::HashMap;

use cx_hmir::{
    HMIRBody, HMIRFnSignature,
    body::HMIRLocalID,
    ty::HMIRTypeID,
    unit::function::HMIRFnPrototype,
    value::{HMIRPlace, HMIRSlotID, HMIRValue},
};
use cx_log::CXResult;
use cx_mir::{MIRBindable, MIRFunction, MIRScopeID};

use crate::{
    env::{HMIREnvironment, function::MIRFunctionBuilder},
    lower::types::lower_prototype,
};

#[derive(Debug)]
pub struct FnLoweringContext<'env, 'global, 'hmir> {
    env: &'env mut HMIREnvironment<'global, 'hmir>,
    signature: &'hmir HMIRFnSignature,
    builder: MIRFunctionBuilder,

    locals: HashMap<HMIRLocalID, HMIRValue>,
    slots: Vec<MIRBindable>,
    open_scopes: Vec<MIRScopeID>,
}

impl<'env, 'global, 'hmir> FnLoweringContext<'env, 'global, 'hmir> {
    pub fn new(
        env: &'env mut HMIREnvironment<'global, 'hmir>,
        body: &HMIRBody,
        prototype: &'hmir HMIRFnPrototype,
    ) -> CXResult<Self> {
        let mir_prototype = lower_prototype(env, body, prototype)?;

        Ok(Self {
            env,
            signature: prototype.signature(),
            builder: MIRFunctionBuilder::new(mir_prototype),

            locals: HashMap::new(),
            slots: Vec::new(),
            open_scopes: Vec::new(),
        })
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

    pub fn bind(&mut self, ty: HMIRTypeID, binding: MIRBindable) -> HMIRValue {
        self.slots.push(binding);
        let slot = HMIRSlotID::new(self.slots.len() - 1);

        HMIRValue::place(ty, HMIRPlace::Local(slot))
    }

    pub fn slot(&self, slot: HMIRSlotID) -> &MIRBindable {
        &self.slots[slot.index()]
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

    pub fn finish(self) -> MIRFunction {
        self.builder.finish()
    }
}
