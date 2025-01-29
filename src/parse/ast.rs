use std::fmt::Debug;
use crate::lex::token::KeywordType;
use crate::parse::val_type::ValType;

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
    pub fn_declarations: Vec<FunctionDeclaration>,
}

impl Node for Root {
    fn print(&self, indent: usize) {
        for fn_decl in &self.fn_declarations {
            fn_decl.print(indent);
        }
    }
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub return_type: KeywordType,
    pub name: String,
    pub body: Vec<Expression>
}

impl Node for FunctionDeclaration {
    fn print(&self, indent: usize) {
        println!("{:indent$}{:?} {}", "", self.return_type, self.name, indent = indent);
        for stmt in &self.body {
            stmt.print(indent + 4);
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

    VariableDeclaration {
        type_: ValType,
        name: String
    },

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
                println!("{:indent$}else", "", indent = indent);
                for stmt in else_ {
                    stmt.print(indent + 4);
                }
            },
            _ => println!("{:indent$}{:?}", "", self, indent = indent)
        }
    }
}