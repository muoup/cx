use std::env;
use crate::parse::ast::Node;

mod lex;
mod parse;
mod codegen;
mod preprocessor;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let Some(file_name) = args.get(1) else {
        println!("Usage: {} <file>", args[0]);
        return;
    };

    let source = std::fs::read_to_string(file_name).unwrap();
    let preprocessed = preprocessor::preprocess(&source);
    let mut lexer = lex::generate_tokens(preprocessed.as_str());

    if let Some(ast) = parse::parser::parse_ast(&mut lexer) {
        codegen::codegen::ast_codegen(&ast);
    }
}
