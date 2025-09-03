use speedy::{Readable, Writable};
use cx_data_ast::parse::ast::{CXBinOp, CXCastType, CXUnOp};
use cx_data_ast::parse::identifier::CXIdent;
use crate::cx_types::{CXFunctionPrototype, CXType};
use crate::{CXFnData, CXFnMap, CXTypeData, CXTypeMap};

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCStructureData {
    pub type_data: CXTypeData,
    pub fn_data: CXFnData,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCAST {
    pub source_file: String,

    pub type_map: CXTypeMap,
    pub fn_map: CXFnMap,

    pub destructors_required: Vec<CXType>,
    pub function_defs: Vec<TCFunctionDef>,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct FunctionTemplateRequest {
    pub module_origin: Option<CXIdent>,
    pub name: CXIdent,
    pub type_arguments: Vec<CXType>
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCFunctionDef {
    pub prototype: CXFunctionPrototype,
    pub body: Box<TCExpr>
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

    VariableReference {
        name: CXIdent
    },

    FunctionReference {
        name: CXIdent
    },

    MemberFunctionReference {
        target: Box<TCExpr>,
        name: CXIdent,
    },

    FunctionCall {
        function: Box<TCExpr>,
        arguments: Vec<TCExpr>,
        direct_call: bool
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

    TemporaryBuffer {
        _type: CXType
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

    DeconstructObject {
        variable_name: CXIdent,
        variable_type: CXType,
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCInitIndex {
    pub name: Option<String>,
    pub value: TCExpr,
    pub index: usize,
}