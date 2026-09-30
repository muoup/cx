use crate::{
    binding::{HMIRHole, HMIRHoleID, HMIRLocal, HMIRLocalID},
    expr::kind::{HMIRExpr, HMIRExprID},
};

#[derive(Debug, Clone, Default)]
pub struct HMIRBody {
    exprs: Vec<HMIRExpr>,
    locals: Vec<HMIRLocal>,
    holes: Vec<HMIRHole>,
}

impl HMIRBody {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_expr(&mut self, expr: HMIRExpr) -> HMIRExprID {
        self.exprs.push(expr);
        HMIRExprID::new(self.exprs.len() - 1)
    }

    pub fn declare_local(&mut self, local: HMIRLocal) -> HMIRLocalID {
        self.locals.push(local);
        HMIRLocalID::new(self.locals.len() - 1)
    }

    pub fn declare_hole(&mut self, hole: HMIRHole) -> HMIRHoleID {
        self.holes.push(hole);
        HMIRHoleID::new(self.holes.len() - 1)
    }

    pub fn expr(&self, id: HMIRExprID) -> &HMIRExpr {
        &self.exprs[id.index()]
    }

    pub fn expr_mut(&mut self, id: HMIRExprID) -> &mut HMIRExpr {
        &mut self.exprs[id.index()]
    }

    pub fn local(&self, id: HMIRLocalID) -> &HMIRLocal {
        &self.locals[id.index()]
    }

    pub fn local_mut(&mut self, id: HMIRLocalID) -> &mut HMIRLocal {
        &mut self.locals[id.index()]
    }

    pub fn hole(&self, id: HMIRHoleID) -> &HMIRHole {
        &self.holes[id.index()]
    }
}
