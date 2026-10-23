use std::collections::{HashMap, HashSet};

use cx_hmir::{
    HMIRDefID, HMIRUnitID,
    expr::HMIRExprID,
    ty::{HMIRType, HMIRTypeID},
};
use cx_mir::MIRTypeID;

use crate::symbol::{
    SymbolState,
    recursive::{ProvisionalType, TypeFrame},
};

const REALIZED_BIT: usize = 1 << 63;

#[derive(Debug)]
pub(crate) struct HMIRResolver {
    realized: HashMap<HMIRTypeID, HMIRType>,
    declared: HashMap<HMIRExprID, HMIRTypeID>,
    incomplete: HashSet<HMIRTypeID>,
    lowered: HashMap<HMIRTypeID, MIRTypeID>,
    symbols: HashMap<HMIRDefID, SymbolState>,
    pub imported: HashMap<(HMIRUnitID, HMIRTypeID), HMIRTypeID>,
    pub defining_types: Vec<TypeFrame>,
    pub active_symbols: Vec<HMIRDefID>,
}

impl HMIRResolver {
    pub fn new() -> Self {
        Self {
            realized: HashMap::new(),
            declared: HashMap::new(),
            incomplete: HashSet::new(),
            lowered: HashMap::new(),
            symbols: HashMap::new(),
            imported: HashMap::new(),
            defining_types: Vec::new(),
            active_symbols: Vec::new(),
        }
    }

    pub fn realize_type(&mut self, ty: HMIRType) -> HMIRTypeID {
        let id = HMIRTypeID::new(self.realized.len() | REALIZED_BIT);
        self.realized.insert(id, ty);
        id
    }

    pub fn publish_type(&mut self, ty: HMIRTypeID) {
        let pending = self
            .defining_types
            .iter()
            .filter(|frame| {
                matches!(
                    self.symbols.get(&frame.def),
                    Some(SymbolState::InProgress(None))
                )
            })
            .map(|frame| (frame.def, frame.constructors.clone()))
            .collect::<Vec<_>>();
        for (id, constructors) in pending {
            let provisional = constructors
                .into_iter()
                .rev()
                .fold(ty, |inner, constructor| constructor.apply(self, inner));
            self.symbols.insert(
                id,
                SymbolState::InProgress(Some(ProvisionalType {
                    ty: provisional,
                    reservation: ty,
                })),
            );
        }
    }

    pub fn forget_symbol(&mut self, id: HMIRDefID) {
        self.symbols.remove(&id);
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

    pub fn symbol_state(&self, def: HMIRDefID) -> Option<&SymbolState> {
        self.symbols.get(&def)
    }

    pub fn set_symbol_state(&mut self, def: HMIRDefID, state: SymbolState) {
        self.symbols.insert(def, state);
    }
}
