use cx_hir::ast::types::HIRTagKind;
use cx_intrinsics::Intrinsic;
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    HMIRDefID, HMIROp, HMIRPattern,
    body::{HMIRBodyID, HMIRHoleID, HMIRLocalID},
    expr::literal::HMIRLiteral,
    ty::{HMIRExprType, HMIRTypeID},
};

pub mod aggregate;
pub mod literal;
pub mod op;

pub type HMIRIntrinsic = Intrinsic<HMIRExprID, HMIRExprID>;

#[derive(Debug, Clone)]
pub struct HMIRExpr {
    kind: HMIRExprKind,
    span: TokenRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HMIRExprID {
    body: HMIRBodyID,
    index: u32,
}

#[derive(Debug, Clone)]
pub enum HMIRExprKind {
    Literal(HMIRLiteral),
    // A type of the unit's table; input HMIR never holds the slots or addresses of an evaluation
    TypeRef(HMIRTypeID),
    Local(HMIRLocalID),
    Def(HMIRDefID),
    Name {
        name: QualifiedName,
        tag: Option<HIRTagKind>,
    },
    Hole(HMIRHoleID),
    Type(Box<HMIRExprType>),

    Comptime(HMIRExprID),
    Quote {
        params: Vec<HMIRLocalID>,
        body: HMIRExprID,
    },
    Splice {
        quote: HMIRExprID,
        args: Vec<HMIRExprID>,
    },

    Intrinsic(HMIRIntrinsic),
    Op(HMIROp),
    Error(CXIdent),

    Let {
        local: HMIRLocalID,
        initializer: Option<HMIRExprID>,
    },
    Call {
        callee: HMIRExprID,
        args: Vec<HMIRExprID>,
    },
    Piped(HMIRExprID),

    Block {
        statements: Vec<HMIRExprID>,
        scoped: bool,
        yields: bool,
    },
    If {
        condition: HMIRExprID,
        then_branch: HMIRExprID,
        else_branch: Option<HMIRExprID>,
    },
    While {
        condition: HMIRExprID,
        body: HMIRExprID,
        pre_eval: bool,
    },
    For {
        init: HMIRExprID,
        condition: HMIRExprID,
        increment: HMIRExprID,
        body: HMIRExprID,
    },
    Switch {
        condition: HMIRExprID,
        body: HMIRExprID,
    },
    // A label of the innermost enclosing switch; 'default' when it has no value
    Case {
        value: Option<HMIRExprID>,
        body: HMIRExprID,
    },
    Match {
        scrutinee: HMIRExprID,
        subject: HMIRLocalID,
        arms: Vec<(HMIRPattern, HMIRExprID)>,
    },
    Label {
        name: CXIdent,
        body: HMIRExprID,
    },
}

impl HMIRExprID {
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

impl HMIRExpr {
    pub fn new(kind: HMIRExprKind, span: TokenRange) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &HMIRExprKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut HMIRExprKind {
        &mut self.kind
    }

    pub fn span(&self) -> &TokenRange {
        &self.span
    }
}
