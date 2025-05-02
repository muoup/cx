use crate::lex::token::OperatorType;
use crate::parse::macros::error_pointer;
use crate::parse::parser::ParserData;
use crate::parse::pass_unverified::global_scope::parse_global_stmt;
use crate::parse::value_type::CXValType;
use crate::util::dump_all;

mod expression;
mod global_scope;
mod format;
mod typing;

pub fn generate_unverified(parser_data: &mut ParserData) -> Option<UVAST> {
    let mut stmts = Vec::new();

    while parser_data.toks.has_next() {
        let Some(stmt) = parse_global_stmt(parser_data) else {
            eprintln!("PARSER ERROR: Unverified parsing failed, dumping correctly parsed expressions.");
            eprintln!("{}", error_pointer(&parser_data.toks));
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
        type_: CXValType
    },

    SingleExpression {
        expression: UVExpr
    },

    BodiedExpression {
        header: UVExpr,
        body: UVExpr
    },

    Function {
        name: String,
        params: Vec<(String, CXValType)>,
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
    Identifier(String),
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