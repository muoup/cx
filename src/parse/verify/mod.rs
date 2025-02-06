use crate::parse::ast::AST;

mod context;
mod global_pass;
mod local_pass;

pub fn verify_ast(ast: &mut AST) -> Option<()> {
    let mut context = context::VerifyContext::new();

    global_pass::global_pass(&mut context, &ast.root);
    local_pass::local_pass(&mut context, &mut ast.root)?;

    Some(())
}