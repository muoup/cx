use std::collections::HashMap;

use cx_hmir::{body::HMIRLocalID, expr::HMIRExprID, ty::HMIRTypeID};
use cx_util::identifier::CXIdent;
use index_vec::IndexVec;

use crate::value::{HMIRSlotID, HMIRValue, constant::HMIRConstant};

#[derive(Debug, Default)]
pub struct ComptimeContext {
    locals: HashMap<HMIRLocalID, HMIRValue>,
    yields: usize,
    switches: Vec<Option<HMIRExprID>>,
    slots: IndexVec<HMIRSlotID, ComptimeSlot>,
}

#[derive(Debug)]
pub struct ComptimeSlot {
    ty: HMIRTypeID,
    name: Option<CXIdent>,
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

    pub fn begin_yield(&mut self) {
        self.yields += 1;
    }

    pub fn end_yield(&mut self) {
        self.yields -= 1;
    }

    pub fn yielding(&self) -> bool {
        self.yields != 0
    }

    pub fn begin_switch(&mut self, selected: HMIRExprID) {
        self.switches.push(Some(selected));
    }

    pub fn end_switch(&mut self) {
        self.switches.pop().expect("a switch is open");
    }

    pub fn selected_case(&self) -> Option<HMIRExprID> {
        self.switches.last().copied().flatten()
    }

    pub fn activate_case(&mut self) {
        *self.switches.last_mut().expect("a switch is open") = None;
    }

    pub fn in_switch(&self) -> bool {
        !self.switches.is_empty()
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
        &self.slots[slot]
    }

    pub fn slot_mut(&mut self, slot: HMIRSlotID) -> &mut ComptimeSlot {
        &mut self.slots[slot]
    }

    pub fn allocate_slot(&mut self, ty: HMIRTypeID, name: Option<CXIdent>) -> HMIRSlotID {
        self.slots.push(ComptimeSlot {
            ty,
            name,
            state: ComptimeSlotState::Uninitialized,
        })
    }
}

impl ComptimeSlot {
    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn state(&self) -> &ComptimeSlotState {
        &self.state
    }

    pub fn state_mut(&mut self) -> &mut ComptimeSlotState {
        &mut self.state
    }
}
