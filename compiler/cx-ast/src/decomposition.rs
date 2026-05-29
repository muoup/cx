// CX AST
//  - Information (External symbols + Types)
//  - Defined Symbols

use crate::ast::{expression::CXExpression, function::CXFunctionPrototype, modifiers::CXLinkageMode};

#[derive(Debug)]
pub struct CXGenerationAST {
    pub generation_stmts: Vec<CXGenerationStmt>,
}

#[derive(Debug)]
pub enum CXGenerationStmt {
    Function {
        prototype: CXFunctionPrototype,
        body: Box<CXExpression>
    },

    AddressableGlobal {
        name: String,
        initializer: Option<CXExpression>,
        linkage: CXLinkageMode,
    }
}