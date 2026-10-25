use cx_hmir::{expr::HMIRExprID, ty::HMIRTypeID};
use cx_util::identifier::CXIdent;
use index_vec::IndexVec;

use crate::{
    env::function::scope::Scopes,
    value::{HMIRSlotID, constant::HMIRConstant},
};

#[derive(Debug, Default)]
pub struct ComptimeContext {
    scopes: Scopes<ComptimeScope>,
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

    pub(crate) fn scopes(&self) -> &Scopes<ComptimeScope> {
        &self.scopes
    }

    pub(crate) fn scopes_mut(&mut self) -> &mut Scopes<ComptimeScope> {
        &mut self.scopes
    }

    pub(crate) fn yielding(&self) -> bool {
        self.scopes.controls().any(|(_, scope)| scope.yields)
    }

    pub(crate) fn switch(&self) -> Option<Switch> {
        self.scopes.controls().rev().find_map(|(_, scope)| scope.switch)
    }

    pub(crate) fn selected_case(&self) -> Option<HMIRExprID> {
        match self.switch() {
            Some(Switch::Seeking(selected)) => Some(selected),
            _ => None,
        }
    }

    pub(crate) fn activate_case(&mut self) {
        let (index, _) = self
            .scopes
            .controls()
            .rev()
            .find(|(_, scope)| scope.switch.is_some())
            .expect("a switch is open");
        self.scopes.control_mut(index).switch = Some(Switch::Active);
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

#[derive(Debug, Clone, Copy)]
pub(crate) enum Switch {
    Seeking(HMIRExprID),
    Active,
}

#[derive(Debug, Default)]
pub(crate) struct ComptimeScope {
    yields: bool,
    switch: Option<Switch>,
}

impl ComptimeScope {
    pub(crate) fn begin_yield(&mut self) {
        self.yields = true;
    }

    pub(crate) fn begin_switch(&mut self, selected: HMIRExprID) {
        self.switch = Some(Switch::Seeking(selected));
    }
}
