use std::collections::HashMap;
use uuid::Uuid;
use serde::{Deserialize, Serialize};
use crate::lex::token::Token;
use crate::parse::value_type::{CXType, CXTypeKind};
use crate::parse::identifier::CXIdent;

pub type CXTypeMap = HashMap<String, CXType>;
pub type CXFunctionMap = HashMap<String, CXFunctionPrototype>;

#[derive(Debug)]
pub struct CXAST {
    pub tokens: Vec<Token>,

    // Path to .cx file
    pub file_path: String,
    
    // Prefix for internal paths (i.e. {internal_path}.[o|cx-types|cx-functions])
    pub internal_path: String,
    
    pub imports: Vec<String>,

    pub global_stmts: Vec<CXGlobalStmt>,
    
    pub public_functions: Vec<String>,

    pub type_map: CXTypeMap,
    pub function_map: CXFunctionMap,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CXParameter {
    pub name: Option<CXIdent>,
    pub type_: CXType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CXFunctionPrototype {
    pub name: CXIdent,
    pub return_type: CXType,
    pub params: Vec<CXParameter>,
    pub var_args: bool
}

#[derive(Debug, Serialize, Deserialize)]
pub enum CXGlobalStmt {
    GlobalVariable {
        name: CXIdent,
        type_: CXType,
        initializer: Option<CXExpr>
    },

    FunctionDefinition {
        prototype: CXFunctionPrototype,
        body: Box<CXExpr>,
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CXUnOp {
    Dereference, AddressOf,
    Negative,
    BNot, LNot,
    ArrayIndex,
    InitializerIndex,
    
    ExplicitCast(CXType),

    PreIncrement(i8),
    PostIncrement(i8),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CXInitIndex {
    Named(CXIdent, Box<CXExpr>),
    Unnamed(Box<CXExpr>)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CXExpr {
    pub uuid: u64,
    pub kind: CXExprKind,

    pub start_index: usize,
    pub end_index: usize,
}

impl Default for CXExpr {
    fn default() -> Self {
        CXExpr {
            uuid: 0,
            kind: CXExprKind::Taken,

            start_index: 0,
            end_index: 0,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CXExprKind {
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
    Switch {
        condition: Box<CXExpr>,
        block: Vec<CXExpr>,
        cases: Vec<(u64, usize)>,
        default_case: Option<usize>
    },

    SizeOf {
        expr: Box<CXExpr>
    },
    VarDeclaration {
        type_: CXType,
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
    
    Defer {
        expr: Box<CXExpr>
    },

    ImplicitCast {
        expr: Box<CXExpr>,
        from_type: CXType,
        to_type: CXType,
        cast_type: CXCastType
    },

    ImplicitLoad {
        expr: Box<CXExpr>,
        loaded_type: CXType
    },
    
    Move {
        expr: Box<CXExpr>
    },

    GetFunctionAddr {
        func_name: Box<CXExpr>,
        func_sig: CXType
    },

    InitializerList {
        indices: Vec<CXInitIndex>,
    }
}

impl CXExprKind {
    pub fn into_expr(self, start_index: usize, end_index: usize) -> CXExpr {
        CXExpr {
            uuid: Uuid::new_v4().as_u128() as u64,
            kind: self,

            start_index,
            end_index,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CXCastType {
    IntegralCast,
    FloatCast,
    IntToFloat,
    FloatToInt,
    BitCast,
    IntegralTrunc,
    IntToPtrDiff,
    PtrToInt,
    IntToPtr,
    FunctionToPointerDecay,
    
    AddPointerTag,
    RemovePointerTag
}