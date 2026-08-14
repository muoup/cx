// CX AST
//  - Information (External symbols + Types)
//  - Defined Symbols

use cx_util::identifier::CXIdent;

use crate::ast::{
    expression::HIRExpression,
    function::HIRFunctionPrototype,
    modifiers::{LinkageMode, HIRSymbolNameScheme},
    types::HIRType,
};

#[derive(Debug)]
pub struct HIRGenerationAST {
    pub generation_stmts: Vec<HIRGenerationStmt>,
}

#[derive(Debug)]
pub enum HIRGenerationStmt {
    Function {
        prototype: HIRFunctionPrototype,
        body: Box<HIRExpression>,
    },

    AddressableGlobal {
        name: CXIdent,
        _type: HIRType,
        initializer: Option<HIRExpression>,
        linkage: LinkageMode,
        symbol_naming: HIRSymbolNameScheme,
    },

    StringLiteral {
        name: CXIdent,
        value: String,
    },
}
