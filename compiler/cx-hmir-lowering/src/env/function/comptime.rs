use rustc_hash::FxHashMap;

use cx_hmir::{expr::HMIRExprID, ty::HMIRTypeID};
use cx_util::identifier::CXIdent;

use crate::{env::function::scope::Scopes, value::HMIRRegionID};

#[derive(Debug, Default)]
pub struct ComptimeContext {
    scopes: Scopes<ComptimeScope>,
    slots: FxHashMap<HMIRRegionID, ComptimeSlot>,
    function: String,
}

#[derive(Debug)]
pub struct ComptimeSlot {
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    state: ComptimeSlotState,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComptimeSlotState {
    Uninitialized,
    Live,
    Invalidated,
}

impl ComptimeContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub(crate) fn of(function: String) -> Self {
        Self {
            function,
            ..Self::default()
        }
    }

    pub(crate) fn function(&self) -> &str {
        &self.function
    }

    pub(crate) fn unconsumed(&self, nodrop: impl Fn(HMIRTypeID) -> bool) -> Option<&ComptimeSlot> {
        self.slots
            .iter()
            .filter(|(_, slot)| slot.state == ComptimeSlotState::Live && nodrop(slot.ty))
            .min_by_key(|(region, _)| **region)
            .map(|(_, slot)| slot)
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

    pub fn slot(&self, region: HMIRRegionID) -> Option<&ComptimeSlot> {
        self.slots.get(&region)
    }

    pub fn slot_mut(&mut self, region: HMIRRegionID) -> Option<&mut ComptimeSlot> {
        self.slots.get_mut(&region)
    }

    pub fn declare_slot(&mut self, region: HMIRRegionID, ty: HMIRTypeID, name: Option<CXIdent>) {
        let state = ComptimeSlotState::Uninitialized;
        self.slots.insert(region, ComptimeSlot { ty, name, state });
    }
}

impl ComptimeSlot {
    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn state(&self) -> ComptimeSlotState {
        self.state
    }

    pub fn set_state(&mut self, state: ComptimeSlotState) {
        self.state = state;
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
