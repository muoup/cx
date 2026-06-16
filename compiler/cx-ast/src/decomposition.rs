// CX AST
//  - Information (External symbols + Types)
//  - Defined Symbols

use cx_util::identifier::CXIdent;

use crate::ast::{
    expression::CXExpression, function::CXFunctionPrototype, modifiers::CXLinkageMode,
    types::CXType,
};

#[derive(Debug)]
pub struct CXGenerationAST {
    pub generation_stmts: Vec<CXGenerationStmt>,
}

#[derive(Debug)]
pub enum CXGenerationStmt {
    Function {
        prototype: CXFunctionPrototype,
        body: Box<CXExpression>,
    },

    AddressableGlobal {
        name: CXIdent,
        _type: CXType,
        initializer: Option<CXExpression>,
        linkage: CXLinkageMode,
    },

    StringLiteral {
        name: CXIdent,
        value: String,
    },
}
