use std::path::PathBuf;

use cx_namespace::module::NamespacePath;
pub use cx_preparse_data::Import as HIRImport;
use cx_preparse_data::NamespaceAliases;
use cx_util::{identifier::CXIdent};

use crate::ast::{
    expression::HIRExpression,
    function::{HIRComptimeFnPrototype, HIRFunctionPrototype},
    global_var::HIRGlobalVariable,
    modifiers::VisibilityMode,
    template::HIRTemplatePrototype,
    types::{HIRTagKind, HIRType},
};

pub mod expression;
pub mod function;
pub mod global_var;
pub mod modifiers;
pub mod pattern;
pub mod template;
pub mod types;

#[derive(Debug)]
pub struct HIR {
    pub module_path: PathBuf,
    pub imports: Vec<HIRImport>,
    pub definition_stmts: Vec<HIRDefinition>,
    pub namespace_aliases: NamespaceAliases,
}

#[derive(Debug)]
pub struct HIRDefinition {
    pub namespace: NamespacePath,
    pub stmt: HIRStmt,
}

#[derive(Debug)]
pub enum HIRStmt {
    TypeDefinition {
        name: Option<CXIdent>,
        visibility: VisibilityMode,
        template_prototype: Option<HIRTemplatePrototype>,
        _type: HIRType,
        tag: Option<HIRTagKind>,
    },

    FunctionDefinition {
        prototype: HIRFunctionPrototype,
        visibility: VisibilityMode,
        template_prototype: Option<HIRTemplatePrototype>,
        body: Option<Box<HIRExpression>>,
    },

    ComptimeFunctionDefinition {
        prototype: HIRComptimeFnPrototype,
        visibility: VisibilityMode,
        template_prototype: Option<HIRTemplatePrototype>,
        body: Box<HIRExpression>,
    },

    GlobalVariableDefinition {
        visibility: VisibilityMode,
        variable: HIRGlobalVariable,
    },
}

impl HIR {
    pub fn new(module_path: PathBuf, imports: Vec<HIRImport>) -> Self {
        Self {
            module_path,
            imports,
            definition_stmts: Vec::new(),
            namespace_aliases: NamespaceAliases::new(),
        }
    }
}
