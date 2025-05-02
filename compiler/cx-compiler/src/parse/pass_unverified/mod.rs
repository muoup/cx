use std::fmt::Display;
use crate::lex::token::OperatorType;
use crate::parse::macros::error_pointer;
use crate::parse::parser::ParserData;
use crate::parse::pass_molded::CXParameter;
use crate::parse::pass_unverified::global_scope::parse_global_stmt;
use crate::parse::value_type::CXValType;
use crate::util::{dump_all, dump_data};

mod expression;
mod global_scope;
mod format;
mod typing;

pub fn generate_unverified(parser_data: &mut ParserData) -> Option<UVAST> {
    let mut stmts = Vec::new();

    while parser_data.toks.has_next() {
        let Some(stmt) = parse_global_stmt(parser_data) else {
            return None;
        };

        dump_data(&stmt);
        dump_data(&"\n".to_string());

        stmts.push(stmt);
    }

    Some(
        UVAST {
            stmts
        }
    )
}

#[derive(Debug)]
pub struct UVAST {
    pub stmts: Vec<UVGlobalStmt>
}

#[derive(Debug)]
pub enum UVGlobalStmt {
    Import(String),

    TypeDeclaration {
        name: String,
        type_: CXValType
    },

    Function {
        name: UVIdent,
        params: Vec<CXParameter>,
        return_type: CXValType,
        body: Option<UVExpr>
    },

    HandledInternally
}

#[derive(Debug, PartialEq, Clone)]
pub enum UVOp {
    Assignment(Option<OperatorType>),
    BinOp(OperatorType),
    UnOpPre(OperatorType),
    UnOpPost(OperatorType),
}

#[derive(Debug)]
pub enum UVExpr {
    Identifier(UVIdent),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    Compound {                  // expr1 expr2 (int x, x(a,b,c) x[1], etc)
        left: Box<UVExpr>,
        right: Box<UVExpr>,
    },

    If {
        condition: Box<UVExpr>,
        then_branch: Box<UVExpr>,
        else_branch: Option<Box<UVExpr>>
    },

    While {
        condition: Box<UVExpr>,
        body: Box<UVExpr>
    },

    For {
        init: Option<Box<UVExpr>>,
        condition: Option<Box<UVExpr>>,
        increment: Option<Box<UVExpr>>,
        body: Box<UVExpr>
    },

    Return {
        value: Option<Box<UVExpr>>
    },

    Complex {
        op_stack: Vec<UVOp>,
        expr_stack: Vec<UVExpr>
    },

    ExprChain(Vec<UVExpr>),

    Parenthesized(Option<Box<UVExpr>>),
    Braced(Box<UVExpr>)
}

#[derive(Debug)]
pub enum UVIdent {
    Identifier(String),
    ScopedIdentifier(Vec<String>),
}

impl Display for UVIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UVIdent::Identifier(name) => write!(f, "{}", name),
            UVIdent::ScopedIdentifier(names) => write!(f, "{}", names.join("::")),
        }
    }
}