use crate::parse::macros::error_pointer;
use crate::parse::parser::ParserData;
use crate::parse::pass_unverified::global_scope::parse_global_stmt;
use crate::parse::value_type::ValueType;
use crate::util::dump_all;

mod expression;
mod global_scope;
mod operators;
mod format;
mod typing;

pub fn generate_unverified(parser_data: &mut ParserData) -> Option<UVAST> {
    let mut stmts = Vec::new();

    while parser_data.toks.has_next() {
        let starting_token = parser_data.toks.peek().unwrap().clone();

        let Some(stmt) = parse_global_stmt(parser_data) else {
            eprintln!("PARSER ERROR: Unverified parsing failed, dumping correctly parsed expressions.");
            eprintln!("{}", error_pointer(parser_data));
            dump_all(stmts);
            return None;
        };

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
        type_: ValueType
    },

    SingleExpression {
        expression: UVExpr
    },

    BodiedExpression {
        header: UVExpr,
        body: UVExpr
    },

    HandledInternally
}

#[derive(Debug, PartialEq, Clone)]
pub enum UVUnOp {
    Dereference,
    Negative,
    BNot,
    LNot,
    ArrayIndex,
    MethodAccess,
    UnaryAccess
}

#[derive(Debug, PartialEq, Clone)]
pub enum UVBinOp {
    Add,
    Subtract, Multiply, Divide, Modulus,
    Less, Greater, LessEqual, GreaterEqual,
    Equal, NotEqual,

    LAnd, LOr, BitAnd, BitOr, BitXor,
    LShift, RShift,

    Comma,

    Access,

    Assignment(Option<Box<UVBinOp>>) // for compound assignment (+=, etc)
}

#[derive(Debug)]
pub enum UVExpr {
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    UnOp {                      // prefix and suffix operators
        operator: UVUnOp,
        operand: Box<UVExpr>
    },
    Parenthesized(Option<Box<UVExpr>>), // (expr)
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
        op_stack: Vec<UVBinOp>,
        expr_stack: Vec<UVExpr>
    },

    ExprChain(Vec<UVExpr>),

    Braced(Box<UVExpr>)
}