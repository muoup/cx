use crate::{
    binding::{HMIRHole, HMIRLocal},
    expr::{meta::HMIRMetaExpr, obj::HMIRObjExpr},
    ids::{HMIRHoleID, HMIRMetaID, HMIRMetaLocalID, HMIRObjID, HMIRObjLocalID},
};

#[derive(Debug, Clone, Default)]
pub struct HMIRBody {
    meta: Vec<HMIRMetaExpr>,
    obj: Vec<HMIRObjExpr>,
    meta_locals: Vec<HMIRLocal>,
    obj_locals: Vec<HMIRLocal>,
    holes: Vec<HMIRHole>,
}

impl HMIRBody {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_meta(&mut self, expr: HMIRMetaExpr) -> HMIRMetaID {
        self.meta.push(expr);
        HMIRMetaID::new(self.meta.len() - 1)
    }

    pub fn push_obj(&mut self, expr: HMIRObjExpr) -> HMIRObjID {
        self.obj.push(expr);
        HMIRObjID::new(self.obj.len() - 1)
    }

    pub fn declare_meta_local(&mut self, local: HMIRLocal) -> HMIRMetaLocalID {
        self.meta_locals.push(local);
        HMIRMetaLocalID::new(self.meta_locals.len() - 1)
    }

    pub fn declare_obj_local(&mut self, local: HMIRLocal) -> HMIRObjLocalID {
        self.obj_locals.push(local);
        HMIRObjLocalID::new(self.obj_locals.len() - 1)
    }

    pub fn declare_hole(&mut self, hole: HMIRHole) -> HMIRHoleID {
        self.holes.push(hole);
        HMIRHoleID::new(self.holes.len() - 1)
    }

    pub fn meta(&self, id: HMIRMetaID) -> &HMIRMetaExpr {
        &self.meta[id.index()]
    }

    pub fn meta_mut(&mut self, id: HMIRMetaID) -> &mut HMIRMetaExpr {
        &mut self.meta[id.index()]
    }

    pub fn obj(&self, id: HMIRObjID) -> &HMIRObjExpr {
        &self.obj[id.index()]
    }

    pub fn obj_mut(&mut self, id: HMIRObjID) -> &mut HMIRObjExpr {
        &mut self.obj[id.index()]
    }

    pub fn meta_local(&self, id: HMIRMetaLocalID) -> &HMIRLocal {
        &self.meta_locals[id.index()]
    }

    pub fn meta_local_mut(&mut self, id: HMIRMetaLocalID) -> &mut HMIRLocal {
        &mut self.meta_locals[id.index()]
    }

    pub fn obj_local(&self, id: HMIRObjLocalID) -> &HMIRLocal {
        &self.obj_locals[id.index()]
    }

    pub fn obj_local_mut(&mut self, id: HMIRObjLocalID) -> &mut HMIRLocal {
        &mut self.obj_locals[id.index()]
    }

    pub fn hole(&self, id: HMIRHoleID) -> &HMIRHole {
        &self.holes[id.index()]
    }
}
