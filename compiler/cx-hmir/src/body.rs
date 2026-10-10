use cx_tokens::TokenRange;
use cx_util::{dense_id, identifier::CXIdent};

use crate::expr::{HMIRExpr, HMIRExprID};

dense_id!(HMIRLocalID, "%");
dense_id!(HMIRHoleID, "?");

#[derive(Debug, Clone, Default)]
pub struct HMIRBody {
    exprs: Vec<HMIRExpr>,
    locals: Vec<HMIRLocal>,
    holes: Vec<HMIRHole>,
}

#[derive(Debug, Clone)]
pub struct HMIRLocal {
    name: Option<CXIdent>,
    ty: HMIRExprID,
    span: TokenRange,

    comptime: bool,
    nodrop: bool,
}

#[derive(Debug, Clone)]
pub struct HMIRHole {
    span: TokenRange,
}

impl HMIRLocal {
    pub fn new(name: Option<CXIdent>, ty: HMIRExprID, span: TokenRange, comptime: bool, nodrop: bool) -> Self {
        Self {
            name,
            ty,
            span,
            comptime,
            nodrop
        }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn ty(&self) -> HMIRExprID {
        self.ty
    }

    pub fn span(&self) -> &TokenRange {
        &self.span
    }

    pub fn comptime(&self) -> bool {
        self.comptime
    }

    pub fn nodrop(&self) -> bool {
        self.nodrop
    }
}

impl HMIRHole {
    pub fn new(span: TokenRange) -> Self {
        Self { span }
    }

    pub fn span(&self) -> &TokenRange {
        &self.span
    }
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
