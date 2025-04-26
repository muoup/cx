mod glob_molding;
mod expr_molding;
mod pattern_molding;
mod operators;
mod format;

use crate::parse::value_type::CXValType;
use crate::parse::pass_molded::glob_molding::mold_globals;
use crate::parse::pass_unverified::{UVExpr, UVAST};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub type TypeMap = HashMap<String, CXValType>;
pub type FunctionMap = HashMap<String, CXFunctionPrototype>;

pub fn mold_ast(ast: &UVAST) -> Option<CXAST> {
    let mut cx_ast = CXAST {
        imports: Vec::new(),
        global_stmts: Vec::new(),
        public_interface: Vec::new(),

        type_map: HashMap::new(),
        function_map: HashMap::new()
    };

    mold_globals(ast, &mut cx_ast)?;

    Some(cx_ast)
}

#[derive(Debug)]
pub struct CXAST<'a> {
    pub imports: Vec<String>,

    pub global_stmts: Vec<CXGlobalStmt>,
    pub public_interface: Vec<&'a CXGlobalStmt>,

    pub type_map: TypeMap,
    pub function_map: FunctionMap,
}

#[derive(Debug, Clone)]
pub struct CXParameter {
    pub name: Option<String>,
    pub type_: CXValType,
}

#[derive(Debug, Clone)]
pub struct CXFunctionPrototype {
    pub name: String,
    pub return_type: CXValType,
    pub parameters: Vec<CXParameter>,
}

#[derive(Debug)]
pub enum CXGlobalStmt {
    GlobalVariable {
        name: String,
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

#[derive(Debug)]
pub enum CXUnOp {
    Dereference, Negative,
    BNot, LNot,
    ArrayIndex,
    InitializerIndex
}

#[derive(Debug, Clone)]
pub enum CXBinOp {
    Add, Subtract, Multiply, Divide, Modulus,
    Less, Greater, LessEqual, GreaterEqual,
    Equal, NotEqual,

    LAnd, LOr, BitAnd, BitOr, BitXor,
    LShift, RShift,

    Comma,

    Access
}

#[derive(Debug)]
pub enum CXInitIndex {
    Named(String, Box<CXExpr>),
    Unnamed(Box<CXExpr>)
}

#[derive(Debug)]
pub enum CXExpr {
    Taken,

    VarReference(String),

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
        body: Box<CXExpr>
    },
    For {
        init: Option<Box<CXExpr>>,
        condition: Option<Box<CXExpr>>,
        increment: Option<Box<CXExpr>>,
        body: Box<CXExpr>
    },

    VarDeclaration {
        type_: CXValType,
        name: String,
        initializer: Option<Box<CXExpr>>
    },
    Assignment {
        lhs: Box<CXExpr>,
        rhs: Box<CXExpr>,
        op: Option<CXBinOp>
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

    StructAccess {
        expr: Box<CXExpr>,
        field: String,
        field_type: CXValType,

        field_offset: usize,
        field_index: usize,
    },

    IndirectFunctionCall {
        callee: Box<CXExpr>,
        args: Vec<CXExpr>
    },

    DirectFunctionCall {
        name: String,
        args: Vec<CXExpr>,
    },

    Block {
        exprs: Vec<CXExpr>,
        value: Option<Box<CXExpr>>
    },

    Return {
        value: Option<Box<CXExpr>>
    },

    ImplicitCast {
        expr: Box<CXExpr>,
        from_type: CXValType,
        to_type: CXValType
    },

    InitializerList {
        indices: Vec<CXInitIndex>,
    }
}