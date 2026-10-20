use std::collections::HashMap;

use cx_hmir::{body::HMIRLocalID, ty::HMIRTypeID};

use crate::value::{HMIRSlotID, HMIRValue, constant::HMIRConstant};

#[derive(Debug, Default)]
pub struct ComptimeContext {
    locals: HashMap<HMIRLocalID, HMIRValue>,
    slots: Vec<ComptimeSlot>,
}

#[derive(Debug)]
pub struct ComptimeSlot {
    ty: HMIRTypeID,
    state: ComptimeSlotState,
}

#[derive(Debug, Clone)]
pub enum ComptimeSlotState {
    Uninitialized,
    Value(HMIRConstant),
    Invalidated,
}

impl ComptimeContext {
    pub fn new() -> Self {
        Self::default()
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

    pub fn remove_local(&mut self, local: HMIRLocalID) {
        self.locals.remove(&local);
    }

    pub fn slots(&self) -> impl Iterator<Item = &ComptimeSlot> {
        self.slots.iter()
    }

    pub fn slot(&self, slot: HMIRSlotID) -> &ComptimeSlot {
        &self.slots[slot.index()]
    }

    pub fn slot_mut(&mut self, slot: HMIRSlotID) -> &mut ComptimeSlot {
        &mut self.slots[slot.index()]
    }

    pub fn allocate_slot(&mut self, ty: HMIRTypeID) -> HMIRSlotID {
        let slot = HMIRSlotID::new(self.slots.len());
        self.slots.push(ComptimeSlot {
            ty,
            state: ComptimeSlotState::Uninitialized,
        });
        slot
    }
}

impl ComptimeSlot {
    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn state(&self) -> &ComptimeSlotState {
        &self.state
    }

    pub fn state_mut(&mut self) -> &mut ComptimeSlotState {
        &mut self.state
    }
}
