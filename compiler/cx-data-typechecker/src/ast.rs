use speedy::{Readable, Writable};
use cx_data_ast::parse::ast::{CXBinOp, CXCastType, CXExpr, CXInitIndex, CXUnOp};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::value_type::CXType;

pub struct TCAST {
    source_file: String,

    statements: Vec<TCGlobalExpr>,
}

pub enum TCGlobalExpr {
    GlobalVariable {
        // TODO
    },

    FunctionDefinition {
        name: CXIdent,
        body: Box<TCExpr>
    },
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCExpr {
    pub _type: CXType,
    pub kind: TCExprKind,
}

impl Default for TCExpr {
    fn default() -> Self {
        TCExpr {
            _type: CXType::default(),
            kind: TCExprKind::Taken,
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum TCExprKind {
    Taken,
    Unit,

    Block {
        statements: Vec<TCExpr>,
    },

    IntLiteral {
        value: i64
    },

    FloatLiteral {
        value: f64
    },

    StringLiteral {
        value: CXIdent
    },

    SizeOf {
        _type: CXType,
    },

    VariableDeclaration {
        type_: CXType,
        name: CXIdent,
    },

    VariableIdentifier {
        name: CXIdent
    },

    FunctionIdentifier {
        name: CXIdent
    },

    FunctionCall {
        function: Box<TCExpr>,
        arguments: Vec<TCExpr>,
    },

    Access {
        target: Box<TCExpr>,
        field: CXIdent,
    },

    Assignment {
        target: Box<TCExpr>,
        value: Box<TCExpr>,
        additional_op: Option<CXBinOp>
    },

    BinOp {
        lhs: Box<TCExpr>,
        rhs: Box<TCExpr>,
        op: CXBinOp
    },

    UnOp {
        operand: Box<TCExpr>,
        operator: CXUnOp
    },

    If {
        condition: Box<TCExpr>,
        then_branch: Box<TCExpr>,
        else_branch: Option<Box<TCExpr>>,
    },

    While {
        condition: Box<TCExpr>,
        body: Box<TCExpr>,
        pre_eval: bool,
    },

    For {
        init: Box<TCExpr>,
        condition: Box<TCExpr>,
        increment: Box<TCExpr>,
        body: Box<TCExpr>,
    },

    Switch {
        condition: Box<TCExpr>,
        block: Vec<TCExpr>,
        cases: Vec<(u64, usize)>, // (case value, index of the case body)
        default_case: Option<usize>,
    },

    ImplicitLoad {
        operand: Box<TCExpr>
    },

    Coercion {
        operand: Box<TCExpr>,
        cast_type: CXCastType
    },

    Defer {
        operand: Box<TCExpr>
    },

    New {
        _type: CXType,
        array_length: Option<Box<TCExpr>>,
    },

    Move {
        operand: Box<TCExpr>
    },

    Return {
        value: Option<Box<TCExpr>>
    },

    InitializerList {
        indices: Vec<TCInitIndex>,
    },

    Break,
    Continue,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCInitIndex {
    pub name: Option<String>,
    pub value: TCExpr,
    pub index: usize,
}