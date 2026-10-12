use std::collections::HashMap;

use cx_hmir::{
    body::HMIRLocalID,
    ty::HMIRTypeID,
    value::{HMIRPlace, HMIRSlotID, HMIRValue},
};
use cx_mir::{MIRBindable, MIRFnPrototype, MIRFunction, MIRInstructionKind, MIRScopeID};
use cx_tokens::TokenRange;

use crate::env::module::MIRFunctionBuilder;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Emission {
    Enabled,
    Suppressed,
}

#[derive(Debug)]
pub struct RuntimeContext {
    builder: MIRFunctionBuilder,
    locals: HashMap<HMIRLocalID, HMIRValue>,
    slots: Vec<MIRBindable>,
    open_scopes: Vec<MIRScopeID>,
    emission: Emission,
}

impl RuntimeContext {
    pub fn new(prototype: MIRFnPrototype) -> Self {
        Self {
            builder: MIRFunctionBuilder::new(prototype),
            locals: HashMap::new(),
            slots: Vec::new(),
            open_scopes: Vec::new(),
            emission: Emission::Enabled,
        }
    }

    pub fn builder(&self) -> &MIRFunctionBuilder {
        &self.builder
    }

    pub fn builder_mut(&mut self) -> &mut MIRFunctionBuilder {
        &mut self.builder
    }

    pub fn emission(&self) -> Emission {
        self.emission
    }

    pub fn set_emission(&mut self, emission: Emission) -> Emission {
        std::mem::replace(&mut self.emission, emission)
    }

    pub fn emit(&mut self, kind: MIRInstructionKind, span: TokenRange) {
        if self.emission == Emission::Suppressed {
            return;
        }
        
        let terminator = kind.is_terminator();
        self.builder.emit(kind, span);
        
        if terminator {
            self.set_emission(Emission::Suppressed);
        }
    }

    pub fn bind(&mut self, ty: HMIRTypeID, binding: MIRBindable) -> HMIRValue {
        let slot = HMIRSlotID::new(self.slots.len());
        self.slots.push(binding);
        HMIRValue::place(ty, HMIRPlace::Local(slot))
    }

    pub fn slots(&self) -> impl Iterator<Item = &MIRBindable> {
        self.slots.iter()
    }

    pub fn slot(&self, slot: HMIRSlotID) -> &MIRBindable {
        &self.slots[slot.index()]
    }

    pub fn slot_mut(&mut self, slot: HMIRSlotID) -> &mut MIRBindable {
        &mut self.slots[slot.index()]
    }

    pub fn allocate_slot(&mut self, bindable: MIRBindable) -> HMIRSlotID {
        let slot = HMIRSlotID::new(self.slots.len());
        self.slots.push(bindable);
        slot
    }
    
    pub fn locals(&self) -> impl Iterator<Item = (&HMIRLocalID, &HMIRValue)> {
        self.locals.iter()
    }

    pub fn local(&self, local: HMIRLocalID) -> Option<&HMIRValue> {
        self.locals.get(&local)
    }

    pub fn set_local(&mut self, local: HMIRLocalID, value: HMIRValue) {
        self.locals.insert(local, value);
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
