use std::collections::HashMap;
use crate::parse::value_type::{CXValType};
use crate::parse::identifier::CXIdent;

pub type TypeMap = HashMap<String, CXValType>;
pub type FunctionMap = HashMap<String, CXFunctionPrototype>;

#[derive(Debug)]
pub struct CXAST {
    pub imports: Vec<String>,

    pub global_stmts: Vec<CXGlobalStmt>,

    pub type_map: TypeMap,
    pub function_map: FunctionMap,
}

#[derive(Debug, Clone)]
pub struct CXParameter {
    pub name: Option<CXIdent>,
    pub type_: CXValType,
}

#[derive(Debug, Clone)]
pub struct CXFunctionPrototype {
    pub name: CXIdent,
    pub return_type: CXValType,
    pub parameters: Vec<CXParameter>,
}

#[derive(Debug)]
pub enum CXGlobalStmt {
    GlobalVariable {
        name: CXIdent,
        type_: CXValType,
        initializer: Option<CXExpr>
    },

    FunctionDefinition {
        prototype: CXFunctionPrototype,
        body: CXExpr,
    },

    FunctionForward {
        prototype: CXFunctionPrototype,
    }
}

#[derive(Debug, Clone)]
pub enum CXUnOp {
    Dereference, AddressOf,
    Negative,
    BNot, LNot,
    ArrayIndex,
    InitializerIndex,

    ExplicitCast(CXValType),

    PreIncrement(i8),
    PostIncrement(i8),
}

#[derive(Debug, Clone)]
pub enum CXBinOp {
    Add, Subtract, Multiply, Divide, Modulus,
    Less, Greater, LessEqual, GreaterEqual,
    Equal, NotEqual,

    LAnd, LOr, BitAnd, BitOr, BitXor,
    LShift, RShift,

    Comma,

    Assign(Option<Box<CXBinOp>>),

    Access, MethodCall, ArrayIndex
}

#[derive(Debug)]
pub enum CXInitIndex {
    Named(CXIdent, Box<CXExpr>),
    Unnamed(Box<CXExpr>)
}

#[derive(Debug)]
pub enum CXExpr {
    Taken,
    Unit,

    Identifier(CXIdent),

    IntLiteral {
        val: i64,
        bytes: u8,
    },
    FloatLiteral {
        val: f64,
        bytes: u8,
    },
    StringLiteral {
        val: String,
    },

    If {
        condition: Box<CXExpr>,
        then_branch: Box<CXExpr>,
        else_branch: Option<Box<CXExpr>>
    },
    While {
        condition: Box<CXExpr>,
        body: Box<CXExpr>,
        pre_eval: bool
    },
    For {
        init: Box<CXExpr>,
        condition: Box<CXExpr>,
        increment: Box<CXExpr>,
        body: Box<CXExpr>
    },

    VarDeclaration {
        type_: CXValType,
        name: CXIdent
    },
    BinOp {
        lhs: Box<CXExpr>,
        rhs: Box<CXExpr>,
        op: CXBinOp
    },
    UnOp {
        operand: Box<CXExpr>,
        operator: CXUnOp
    },

    Block {
        exprs: Vec<CXExpr>,
        value: Option<Box<CXExpr>>
    },

    Break,
    Continue,

    Return {
        value: Option<Box<CXExpr>>
    },

    ImplicitCast {
        expr: Box<CXExpr>,
        from_type: CXValType,
        to_type: CXValType,
        cast_type: CXCastType
    },

    ImplicitLoad {
        expr: Box<CXExpr>,
        loaded_type: CXValType
    },

    GetFunctionAddr {
        func_name: Box<CXExpr>,
        func_sig: CXValType
    },

    InitializerList {
        indices: Vec<CXInitIndex>,
    }
}

#[derive(Debug, Clone)]
pub enum CXCastType {
    IntegralCast,
    FloatCast,
    IntToFloat,
    FloatToInt,
    BitCast,
    IntegralTrunc,
    IntToScaledPtrDiff,
    FunctionToPointerDecay,
}