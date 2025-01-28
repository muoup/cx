use std::fmt::Debug;
use crate::lex::token::KeywordType;

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
    FunctionCall(Box<Expression>, Vec<Expression>),
    Return(Box<Expression>),

    Identifier(String),

    StringLiteral(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    Unit
}

impl Node for Expression {
    fn print(&self, indent: usize) {
        println!("{:indent$}{:?}", "", self, indent = indent);
    }
}