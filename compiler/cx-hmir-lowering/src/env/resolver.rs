use std::collections::{HashMap, HashSet};

use cx_hmir::ty::{HMIRType, HMIRTypeID};
use cx_mir::MIRTypeID;

use crate::{
    env::memo::{MemoContext, MemoContextID, MemoEntry, MemoID, MemoKey},
    value::HMIRValue,
};

pub mod memo;

const REALIZED_BIT: usize = 1 << 63;

#[derive(Debug)]
pub(crate) struct HMIRResolver {
    incomplete: HashSet<HMIRTypeID>,
    realized: HashMap<HMIRTypeID, HMIRType>,
    lowered: HashMap<HMIRTypeID, MIRTypeID>,
    
    memo: HashMap<MemoKey, MemoID>,
    entries: Vec<MemoEntry>,
    contexts: Vec<MemoContext>,
    active: Vec<MemoID>,
}

impl HMIRResolver {
    pub fn new() -> Self {
        Self {
            realized: HashMap::new(),
            incomplete: HashSet::new(),
            lowered: HashMap::new(),
            memo: HashMap::new(),
            entries: Vec::new(),
            contexts: Vec::new(),
            active: Vec::new(),
        }
    }

    pub fn realize_type(&mut self, ty: HMIRType) -> HMIRTypeID {
        let id = HMIRTypeID::new(self.realized.len() | REALIZED_BIT);
        self.realized.insert(id, ty);
        id
    }

    pub fn intern_context(&mut self, context: MemoContext) -> MemoContextID {
        todo!("Intern immutable binding handles, owner, declaration visibility, expected type, and any owning evaluation")
    }

    pub fn reserve(&mut self, key: MemoKey) -> MemoID {
        todo!("Reuse or allocate a stable expression handle without evaluating it")
    }

    pub fn complete(&mut self, id: MemoID, value: HMIRValue) {
        todo!("Complete the reserved entry with a settled value valid in its owning context")
    }

    pub fn forward(&mut self, alias: MemoID, target: MemoID) {
        todo!("Forward an unresolved entry to another expression without forcing it; diagnose cycles when evaluated")
    }

    pub fn realized_type(&self, ty: HMIRTypeID) -> Option<&HMIRType> {
        self.realized.get(&ty)
    }

    pub fn complete_type(&mut self, id: HMIRTypeID, ty: HMIRType) {
        self.realized.insert(id, ty);
        self.incomplete.remove(&id);
    }

    pub fn mark_incomplete(&mut self, id: HMIRTypeID) {
        self.incomplete.insert(id);
    }

    pub fn is_incomplete(&self, id: HMIRTypeID) -> bool {
        self.incomplete.contains(&id)
    }

    pub fn lowered_type(&self, ty: HMIRTypeID) -> Option<MIRTypeID> {
        self.lowered.get(&ty).copied()
    }

    pub fn cache_lowered(&mut self, ty: HMIRTypeID, mir: MIRTypeID) {
        self.lowered.insert(ty, mir);
    }
}
