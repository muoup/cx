use cx_data_ast::parse::ast::{CXBinOp, CXCastType, CXUnOp};
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

    Assignment {
        target: Box<TCExpr>,
        value: Box<TCExpr>,
    },

    BinaryOp {
        left: Box<TCExpr>,
        right: Box<TCExpr>,
        operator: CXBinOp
    },

    UnaryOp {
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

    Break,
    Continue,

    Defer {
        operand: Box<TCExpr>
    },

    New {
        _type: CXType,
        array_size: Option<Box<TCExpr>>,
    },

    Move {
        operand: Box<TCExpr>
    },

    Return {
        value: Option<Box<TCExpr>>
    },
}