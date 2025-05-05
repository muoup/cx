
use crate::parse::value_type::CXValType;
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::{Add, BitAnd, BitOr, BitXor};
use cranelift::prelude::Block;
use crate::parse::parser::ParserData;
use crate::parse::pass_ast::global_scope::parse_global_stmt;
use crate::parse::pass_ast::identifier::CXIdent;

pub mod expression;
pub mod global_scope;
pub mod typing;
pub mod operators;
pub mod identifier;
pub mod format;

pub type TypeMap = HashMap<String, CXValType>;
pub type FunctionMap = HashMap<String, CXFunctionPrototype>;

pub fn parse_ast(mut data: ParserData) -> Option<CXAST> {
    let mut cx_ast = CXAST {
        imports: Vec::new(),
        global_stmts: Vec::new(),
        public_interface: Vec::new(),

        type_map: HashMap::new(),
        function_map: HashMap::new()
    };

    while data.toks.has_next() {
        parse_global_stmt(&mut data, &mut cx_ast);
    }

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

#[derive(Debug, Clone, Copy)]
pub enum CXUnOp {
    Dereference, AddressOf,
    Negative,
    BNot, LNot,
    ArrayIndex,
    InitializerIndex,

    PreIncrement,
    PostIncrement,
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
        body: Box<CXExpr>
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

    Return {
        value: Option<Box<CXExpr>>
    },

    ImplicitCast {
        expr: Box<CXExpr>,
        from_type: CXValType,
        to_type: CXValType
    },

    ImplicitLoad {
        expr: Box<CXExpr>,
        loaded_type: CXValType
    },

    InitializerList {
        indices: Vec<CXInitIndex>,
    }
}