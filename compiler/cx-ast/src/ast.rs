use cx_util::{identifier::CXIdent, module_path::ModulePath};

use crate::ast::{
    expression::CXExpression, function::CXFunctionPrototype, global_var::CXGlobalVariable,
    modifiers::VisibilityMode, template::CXTemplatePrototype, types::CXType,
};

pub mod expression;
pub mod function;
pub mod global_var;
pub mod modifiers;
pub mod pattern;
pub mod template;
pub mod types;

#[derive(Debug)]
pub struct CXAST {
    pub module_path: ModulePath,
    pub imports: Vec<ModulePath>,
    pub definition_stmts: Vec<CXASTStmt>,
}

#[derive(Debug)]
pub enum CXASTStmt {
    TypeDefinition {
        name: Option<CXIdent>,
        visibility: VisibilityMode,
        template_prototype: Option<CXTemplatePrototype>,
        _type: CXType,
    },

    FunctionDefinition {
        prototype: CXFunctionPrototype,
        visibility: VisibilityMode,
        template_prototype: Option<CXTemplatePrototype>,
        body: Option<Box<CXExpression>>,
    },

    GlobalVariableDefinition {
        name: CXIdent,
        visibility: VisibilityMode,
        variable: CXGlobalVariable,
    },
}

impl CXAST {
    pub fn new(module_path: ModulePath, imports: Vec<ModulePath>) -> Self {
        Self {
            module_path,
            imports,
            definition_stmts: Vec::new(),
        }
    }
}
