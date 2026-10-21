use std::collections::{HashMap, HashSet};

use cx_hmir::{
    expr::HMIRExprID,
    ty::{HMIRType, HMIRTypeID},
};
use cx_mir::MIRTypeID;

const REALIZED_BIT: usize = 1 << 63;

#[derive(Debug)]
pub(crate) struct HMIRResolver {
    realized: HashMap<HMIRTypeID, HMIRType>,
    declared: HashMap<HMIRExprID, HMIRTypeID>,
    incomplete: HashSet<HMIRTypeID>,
    lowered: HashMap<HMIRTypeID, MIRTypeID>,
}

impl HMIRResolver {
    pub fn new() -> Self {
        Self {
            realized: HashMap::new(),
            declared: HashMap::new(),
            incomplete: HashSet::new(),
            lowered: HashMap::new(),
        }
    }

    pub fn realize_type(&mut self, ty: HMIRType) -> HMIRTypeID {
        let id = HMIRTypeID::new(self.realized.len() | REALIZED_BIT);
        self.realized.insert(id, ty);
        id
    }

    pub fn realized_type(&self, ty: HMIRTypeID) -> Option<&HMIRType> {
        self.realized.get(&ty)
    }

    // A declaration is one type however often it is evaluated. 'incomplete' stands for it until it is completed.
    pub fn declare_type(&mut self, declaration: HMIRExprID, incomplete: HMIRType) -> HMIRTypeID {
        let id = self.realize_type(incomplete);
        self.declared.insert(declaration, id);
        self.incomplete.insert(id);
        id
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

    pub fn declared_type(&self, declaration: HMIRExprID) -> Option<HMIRTypeID> {
        self.declared.get(&declaration).copied()
    }

    pub fn lowered_type(&self, ty: HMIRTypeID) -> Option<MIRTypeID> {
        self.lowered.get(&ty).copied()
    }

    pub fn cache_lowered(&mut self, ty: HMIRTypeID, mir: MIRTypeID) {
        self.lowered.insert(ty, mir);
    }
}
