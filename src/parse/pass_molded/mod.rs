mod glob_molding;
mod expr_molding;
mod pattern_molding;
mod operators;
mod format;

use crate::parse::ast::ValueType;
use crate::parse::pass_molded::glob_molding::mold_globals;
use crate::parse::pass_unverified::UVAST;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub type TypeMap = HashMap<String, ValueType>;
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

#[derive(Debug)]
pub struct CXParameter {
    pub name: Option<String>,
    pub type_: ValueType,
}

#[derive(Debug)]
pub struct CXFunctionPrototype {
    pub name: String,
    pub return_type: ValueType,
    pub parameters: Vec<CXParameter>,
}

#[derive(Debug)]
pub enum CXGlobalStmt {
    GlobalVariable {
        name: String,
        type_: ValueType,
        initializer: Option<CXExpr>
    },

    FunctionDefinition {
        name: String,
        return_type: ValueType,
        parameters: Vec<CXParameter>,
        body: CXExpr
    }
}

#[derive(Debug)]
pub enum CXUnOp {
    Dereference, Negative,
    BNot, LNot,
    ArrayIndex, MethodAccess
}

#[derive(Debug)]
pub enum CXBinOp {
    Add, Subtract, Multiply, Divide, Modulus,
    Less, Greater, LessEqual, GreaterEqual,
    Equal, NotEqual,

    LAnd, LOr, BitAnd, BitOr, BitXor,
    LShift, RShift,

    Comma,

    Access,

    Assignment(Option<Box<CXBinOp>>), // for compound assignment (+=, etc)
}

#[derive(Debug)]
pub enum CXLValue {

}

#[derive(Debug)]
pub enum CXExpr {
    Identifier(String),
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
        type_: ValueType,
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
        offset: usize,
        field: String,
        field_index: usize,
    },

    FunctionCall {
        callee: Box<CXExpr>,
        args: Vec<CXExpr>
    },

    Block {
        exprs: Vec<CXExpr>
    },

    Return {
        value: Option<Box<CXExpr>>
    },

    ImplicitCast {
        expr: Box<CXExpr>,
        to_type: ValueType
    }
}