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
    pub fn_declarations: Vec<GlobalStatement>,
}

impl Node for Root {
    fn print(&self, indent: usize) {
        for fn_decl in &self.fn_declarations {
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

#[derive(Debug)]
pub enum Expression {
    FunctionCall {
        name: Box<Expression>,
        args: Vec<Expression>
    },
    Return(Box<Expression>),
    Identifier(String),

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
    },

    VariableDeclaration {
        type_: ValType,
        name: String
    },

    VariableStorage {
        name: String
    },

    BinaryOperation {
        operator: OperatorType,
        left: Box<Expression>,
        right: Box<Expression>
    },
    Assignment {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Option<OperatorType>
    },

    NOP,
    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    Unit
}

impl Node for Expression {
    fn print(&self, indent: usize) {
        match self {
            Expression::If { condition, then, else_ } => {
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