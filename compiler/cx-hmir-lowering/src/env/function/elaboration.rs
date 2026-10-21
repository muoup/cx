use std::cell::Cell;

use cx_hmir::{body::HMIRHoleID, ty::HMIRTypeID};
use cx_util::dense_id;

thread_local! {
    static NEXT_SCOPE: Cell<usize> = Cell::new(0);    
}

dense_id!(ElaborationScopeID, "es.");

#[derive(Debug, Clone)]
pub struct ElaborationScope {
    id: ElaborationScopeID,
    holes: Vec<(HMIRHoleID, HMIRTypeID)>,
}

#[derive(Debug, Clone, Copy)]
pub struct HoleMark(usize);

impl ElaborationScope {
    pub fn new() -> Self {
        let id = NEXT_SCOPE.with(|next| {
            let id = next.get();
            next.set(id + 1);
            id
        });

        Self {
            id: ElaborationScopeID(id),
            holes: Vec::new(),
        }
    }

    pub fn id(&self) -> ElaborationScopeID {
        self.id
    }

    pub fn hole(&self, hole: HMIRHoleID) -> Option<HMIRTypeID> {
        self.holes
            .iter()
            .rev()
            .find(|(bound, _)| *bound == hole)
            .map(|(_, ty)| *ty)
    }

    pub fn bind(&mut self, holes: &[HMIRHoleID], ty: HMIRTypeID) -> HoleMark {
        let mark = HoleMark(self.holes.len());
        self.holes.extend(holes.iter().map(|hole| (*hole, ty)));
        mark
    }

    pub fn release(&mut self, mark: HoleMark) {
        self.holes.truncate(mark.0);
    }
}
