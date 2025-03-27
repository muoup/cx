use crate::parse::ast::ValueType;
use crate::parse::parser::ParserData;
use crate::parse::unverified::expression::parse_expr;
use crate::parse::unverified::global_scope::parse_global_expr;
use crate::parse::verify::context::FunctionPrototype;

mod expression;
mod global_scope;
mod operators;

pub fn generate_unverified(parser_data: &mut ParserData) -> Option<UVGlobalStmt> {
    let ast = parse_global_expr(parser_data)?;

    println!("Parsed global statement: {:#?}", ast);

    Some(ast)
}

#[derive(Debug)]
pub enum UVGlobalStmt {
    TypeDeclaration {
        name: Option<String>,
        type_: ValueType
    },

    SingleExpression {
        expression: UVExpr
    },

    BodiedExpression {
        header: UVExpr,
        body: Vec<UVExpr>
    },

    HandledInternally
}

#[derive(Debug)]
pub enum UVUnOp {
    Dereference,
    Negative,
    BNot,
    LNot,
    ArrayIndex,
    MethodAccess
}

#[derive(Debug)]
pub enum UVBinOp {
    Add,
    Subtract, Multiply, Divide, Modulus,
    Less, Greater, LessEqual, GreaterEqual,
    Equal, NotEqual,

    LAnd, LOr, BitAnd, BitOr, BitXor,
    LShift, RShift,

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
        then_branch: Vec<UVExpr>,
        else_branch: Option<Vec<UVExpr>>
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
        operator_stack: Vec<UVBinOp>,
        expression_stack: Vec<UVExpr>
    }
}