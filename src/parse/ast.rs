use std::env::args;
use crate::lex::token::OperatorType;
use crate::parse::val_type::ValType;
use std::fmt::Debug;

pub trait Node: Debug {
    fn print(&self, indent: usize) where Self: Debug {
        println!("{:indent$}{:?}", "", self, indent = indent);
    }
}

pub struct AST {
    pub root: Root
}

#[derive(Debug)]
pub struct Root {
    pub global_stmts: Vec<GlobalStatement>,
}

impl Node for Root {
    fn print(&self, indent: usize) {
        for fn_decl in &self.global_stmts {
            fn_decl.print(indent);
        }
    }
}

#[derive(Debug)]
pub enum GlobalStatement {
    Function {
        name: String,
        arguments: Vec<(String, ValType)>,
        return_type: ValType,
        body: Option<Vec<Expression>>
    },
}

impl Node for GlobalStatement {
    fn print(&self, indent: usize) {
        match self {
            GlobalStatement::Function { name, arguments, return_type, body } => {
                println!("{:indent$}fn {} -> {:?}({:?})", "", name, return_type, arguments, indent = indent);
                if let Some(body) = body {
                    for stmt in body {
                        stmt.print(indent + 4);
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum LiteralExpression {
    IntLiteral {
        val: i64,
        bytes: u8
    },
    FloatLiteral {
        val: f64,
        bytes: u8
    },
    StringLiteral(String)
}

#[derive(Debug, Clone)]
pub enum ValueExpression {
    DirectFunctionCall {
        name: String,
        args: Vec<Expression>
    },
    UnaryOperation {
        operator: OperatorType,
        operand: Box<Expression>
    },
    BinaryOperation {
        operator: OperatorType,
        left: Box<Expression>,
        right: Box<Expression>
    },
    Assignment {
        left: Box<Expression>,
        right: Box<Expression>,
        operator: Option<OperatorType>
    }
}

#[derive(Debug, Clone)]
pub enum MemoryExpression {
    VariableDeclaration {
        name: String,
        type_: ValType,
    },
    VariableReference {
        name: String
    },
    VariableStorage {
        name: String,
    }
}

#[derive(Debug, Clone)]
pub enum ControlExpression {
    Return(Box<Expression>),
    Continue, Break,
    If {
        condition: Box<Expression>,
        then: Vec<Expression>,
        else_: Vec<Expression>
    },
    ForLoop {
        init: Box<Expression>,
        condition: Box<Expression>,
        increment: Box<Expression>,
        body: Vec<Expression>
    },
    Loop {
        condition: Box<Expression>,
        body: Vec<Expression>,

        evaluate_condition_first: bool
    }
}

#[derive(Debug, Clone)]
pub enum UnverifiedExpression {
    Identifier(String),
    Cast {
        expr: Box<Expression>,
        type_: ValType
    },
    FunctionCall {
        name: Box<Expression>,
        args: Vec<Expression>
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(LiteralExpression),
    Value(ValueExpression),
    Memory(MemoryExpression),
    Control(ControlExpression),

    // Any expression that should be eliminated by the type checker,
    // if the codegen phase encounters this, behavior is undefined (likely a panic)
    Unverified(UnverifiedExpression),

    NOP,
    Unit,
}

impl Node for Expression {
    fn print(&self, indent: usize) {
        match self {
            Expression::Control(
                ControlExpression::If {
                    condition, then, else_
                }
            ) => {
                println!("{:indent$}if", "", indent = indent);
                condition.print(indent + 4);
                println!("{:indent$}then", "", indent = indent);
                for stmt in then {
                    stmt.print(indent + 4);
                }
                if else_.len() == 0 {
                    return;
                }
                println!("{:indent$}else", "", indent = indent);
                for stmt in else_ {
                    stmt.print(indent + 4);
                }
            },
            _ => println!("{:indent$}{:?}", "", self, indent = indent)
        }
    }
}