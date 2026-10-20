use std::cell::Cell;

use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::expr::{HMIRExpr, HMIRExprID};

thread_local! {
    static NEXT_BODY: Cell<usize> = Cell::new(0);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HMIRBodyID(u32);

// An index into one of a body's tables, stamped with the body it belongs to
macro_rules! body_id {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name {
            body: HMIRBodyID,
            index: u32,
        }

        impl $name {
            pub const fn new(body: HMIRBodyID, index: usize) -> Self {
                Self {
                    body,
                    index: index as u32,
                }
            }

            pub const fn body(self) -> HMIRBodyID {
                self.body
            }

            pub const fn index(self) -> usize {
                self.index as usize
            }
        }
    };
}

body_id!(HMIRLocalID);
body_id!(HMIRHoleID);

#[derive(Debug, Clone)]
pub struct HMIRBody {
    id: HMIRBodyID,
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
}

#[derive(Debug, Clone)]
pub struct HMIRHole {
    span: TokenRange,
}

impl HMIRLocal {
    pub fn new(name: Option<CXIdent>, ty: HMIRExprID, span: TokenRange, comptime: bool) -> Self {
        Self {
            name,
            ty,
            span,
            comptime,
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
        Self {
            id: HMIRBodyID(NEXT_BODY.with(|next| {
                let id = next.get();
                next.set(id + 1);
                id as u32
            })),
            exprs: Vec::new(),
            locals: Vec::new(),
            holes: Vec::new(),
        }
    }

    pub fn id(&self) -> HMIRBodyID {
        self.id
    }

    pub fn push_expr(&mut self, expr: HMIRExpr) -> HMIRExprID {
        self.exprs.push(expr);
        HMIRExprID::new(self.id, self.exprs.len() - 1)
    }

    pub fn declare_local(&mut self, local: HMIRLocal) -> HMIRLocalID {
        self.locals.push(local);
        HMIRLocalID::new(self.id, self.locals.len() - 1)
    }

    pub fn declare_hole(&mut self, hole: HMIRHole) -> HMIRHoleID {
        self.holes.push(hole);
        HMIRHoleID::new(self.id, self.holes.len() - 1)
    }

    pub fn expr(&self, id: HMIRExprID) -> &HMIRExpr {
        debug_assert_eq!(id.body(), self.id, "expression used outside of its body");
        &self.exprs[id.index()]
    }

    pub fn expr_mut(&mut self, id: HMIRExprID) -> &mut HMIRExpr {
        debug_assert_eq!(id.body(), self.id, "expression used outside of its body");
        &mut self.exprs[id.index()]
    }

    pub fn local(&self, id: HMIRLocalID) -> &HMIRLocal {
        debug_assert_eq!(id.body(), self.id, "local used outside of its body");
        &self.locals[id.index()]
    }

    pub fn local_mut(&mut self, id: HMIRLocalID) -> &mut HMIRLocal {
        debug_assert_eq!(id.body(), self.id, "local used outside of its body");
        &mut self.locals[id.index()]
    }

    pub fn hole(&self, id: HMIRHoleID) -> &HMIRHole {
        debug_assert_eq!(id.body(), self.id, "hole used outside of its body");
        &self.holes[id.index()]
    }
}
