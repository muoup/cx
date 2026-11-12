use cx_hmir::{
    body::{HMIRBodyID, HMIRLocalID},
    expr::HMIRExprID,
};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Copy)]
pub enum HMIRTypeSubject {
    Expr(HMIRExprID),
    Local(HMIRLocalID),
}

#[derive(Debug, Default)]
pub struct HMIRTypeFacts {
    pub exprs: FxHashMap<HMIRExprID, String>,
    pub locals: FxHashMap<HMIRLocalID, String>,
}

impl HMIRTypeSubject {
    pub fn body(self) -> HMIRBodyID {
        match self {
            HMIRTypeSubject::Expr(id) => id.body(),
            HMIRTypeSubject::Local(id) => id.body(),
        }
    }
}

impl HMIRTypeFacts {
    pub fn insert(&mut self, subject: HMIRTypeSubject, ty: String) {
        match subject {
            HMIRTypeSubject::Expr(id) => self.exprs.insert(id, ty),
            HMIRTypeSubject::Local(id) => self.locals.insert(id, ty),
        };
    }
}
