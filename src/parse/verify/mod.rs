use crate::parse::ast::AST;
use crate::parse::verify::context::FunctionPrototype;
use crate::parse::verify::typeless_declarations::gen_declarations;

pub mod context;
mod typeless_declarations;

#[derive(Debug)]
pub struct VerifiedAST {
    pub funcs: Vec<FunctionPrototype>
}

pub fn verify_ast(ast: AST) -> Option<VerifiedAST> {
    let (mut type_map, mut fn_map) = gen_declarations(&ast)?;

    None
}