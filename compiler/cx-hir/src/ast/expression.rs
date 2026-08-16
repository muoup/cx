use cx_tokens::token::{FloatSuffix, IntegerBase, IntegerSuffix};
use cx_tokens::TokenRange;
use cx_util::{
    identifier::CXIdent,
    namespace::{EnvironmentNamespace, QualifiedName},
    unsafe_float::FloatWrapper,
};
use speedy::{Readable, Writable};

use crate::ast::{pattern::HIRPattern, template::HIRTemplateInput, types::HIRType};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct HIRExpression {
    pub kind: HIRExprKind,
    pub range: TokenRange,
}

impl Clone for HIRExpression {
    fn clone(&self) -> Self {
        HIRExpression {
            kind: self.kind.clone(),
            range: self.range.clone(),
        }
    }
}

impl Default for HIRExpression {
    fn default() -> Self {
        HIRExpression {
            kind: HIRExprKind::Taken,
            range: TokenRange::internal(),
        }
    }
}

impl HIRExpression {
    pub fn token_range(&self) -> &TokenRange {
        &self.range
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HIRExprKind {
    Taken,
    Void,

    Identifier {
        name: QualifiedName,
        template_input: Option<HIRTemplateInput>,
    },

    IntLiteral {
        magnitude: u64,
        base: IntegerBase,
        suffix: IntegerSuffix,
    },
    BoolLiteral(bool),
    FloatLiteral {
        val: FloatWrapper,
        suffix: FloatSuffix,
    },
    StringLiteral {
        val: String,
    },

    If {
        condition: Box<HIRExpression>,
        then_branch: Box<HIRExpression>,
        else_branch: Option<Box<HIRExpression>>,
    },
    Ternary {
        condition: Box<HIRExpression>,
        then_branch: Box<HIRExpression>,
        else_branch: Box<HIRExpression>,
    },
    While {
        condition: Box<HIRExpression>,
        body: Box<HIRExpression>,
        pre_eval: bool,
    },
    For {
        init: Box<HIRExpression>,
        condition: Box<HIRExpression>,
        increment: Box<HIRExpression>,
        body: Box<HIRExpression>,
    },

    Match {
        condition: Box<HIRExpression>,
        arms: Vec<(HIRPattern, HIRExpression)>, // (value, block)
        default: Option<Box<HIRExpression>>,
    },

    Switch {
        condition: Box<HIRExpression>,
        block: Vec<HIRExpression>,
        cases: Vec<(HIRExpression, usize)>, // (constant expression, block index)
        default_case: Option<usize>,
    },

    SizeOfExpr {
        expr: Box<HIRExpression>,
    },
    SizeOfType {
        _type: HIRType,
    },
    AlignOfExpr {
        expr: Box<HIRExpression>,
    },
    AlignOfType {
        _type: HIRType,
    },

    VarDeclaration {
        _type: HIRType,
        name: CXIdent,
        initial_value: Option<Box<HIRExpression>>,
    },
    BinOp {
        lhs: Box<HIRExpression>,
        rhs: Box<HIRExpression>,
        op: HIRBinOp,
    },
    UnOp {
        operand: Box<HIRExpression>,
        operator: HIRUnOp,
    },

    Block {
        exprs: Vec<HIRExpression>,
        creates_scope: bool,
    },

    Defer {
        expr: Box<HIRExpression>,
    },
    StagedExpression {
        params: Vec<CXIdent>,
        body: Box<HIRExpression>,
    },
    Then,

    Break,
    Continue,

    Return {
        value: Option<Box<HIRExpression>>,
    },
    Yield {
        value: Option<Box<HIRExpression>>,
    },
    Emit {
        expr: Box<HIRExpression>,
    },

    Unsafe {
        expr: Box<HIRExpression>,
    },
    Leak {
        expr: Box<HIRExpression>,
    },
    Adopt {
        expr: Box<HIRExpression>,
    },
    Unpack {
        expr: Box<HIRExpression>,
        bindings: Vec<HIRUnpackBinding>,
    },

    InitializerList {
        indices: Vec<HIRInitIndex>,
    },

    VaArg {
        list: Box<HIRExpression>,
        _type: HIRType,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HIRUnOp {
    Dereference,
    AddressOf,
    Negative,
    BNot,
    LNot,

    Move,

    ExplicitCast(HIRType),
    Is(Box<HIRPattern>),

    PreIncrement(i8),
    PostIncrement(i8),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum HIRBinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,

    LAnd,
    LOr,
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,

    Comma,

    Assign(Option<Box<HIRBinOp>>),

    Access,
    MethodCall,
    ArrayIndex,
    Pipe,
    BackwardPipe,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HIRInitIndex {
    pub name: Option<String>,
    pub value: HIRExpression,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HIRUnpackBinding {
    pub field: CXIdent,
    pub binding: CXIdent,
}

impl HIRExprKind {
    pub fn into_expr(
        self,
        start_index: usize,
        end_index: usize,
        namespace: EnvironmentNamespace,
    ) -> HIRExpression {
        let (start_index, end_index) = if start_index > end_index {
            return HIRExpression {
                kind: self,
                range: TokenRange::error("Expression range start is after range end"),
            };
        } else {
            (start_index, end_index)
        };

        HIRExpression {
            kind: self,
            range: TokenRange::new(start_index, end_index, namespace),
        }
    }

    pub fn block_terminating(&self) -> bool {
        matches!(
            self,
            HIRExprKind::Return { .. }
                | HIRExprKind::Yield { .. }
                | HIRExprKind::Break
                | HIRExprKind::Continue
                | HIRExprKind::Taken
        )
    }
}
