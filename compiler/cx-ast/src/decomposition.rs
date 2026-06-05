// CX AST
//  - Information (External symbols + Types)
//  - Defined Symbols

use std::collections::HashMap;

use cx_util::{identifier::CXIdent, namespace::NamespacePath};

use crate::ast::{
    expression::CXExpression, function::CXFunctionPrototype, modifiers::CXLinkageMode,
    types::CXType,
};

#[derive(Debug)]
pub struct CXGenerationAST {
    pub namespace_aliases: HashMap<NamespacePath, NamespacePath>,
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
