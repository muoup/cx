use crate::parse::ast::{GlobalStatement, ValueType, AST};
use crate::parse::verify::context::FunctionPrototype;

#[derive(Debug)]
pub struct ASTInterface {
    pub functions: Vec<FunctionPrototype>,
    pub types: Vec<(String, ValueType)>,
}

pub(crate) fn from_ast(ast: &AST) -> ASTInterface {
    let mut functions = Vec::new();
    let mut types = Vec::new();

    for stmt in &ast.statements {
        match stmt {
            GlobalStatement::Function { prototype, .. } => {
                functions.push(prototype.clone())
            }
            GlobalStatement::TypeDeclaration(type_def, ..) => {
                types.push((type_def.name.clone(), type_def.value_type.clone()));
            }
            _ => {}
        }
    }

    ASTInterface { functions, types }
}