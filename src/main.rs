use std::env;
use cranelift::codegen;

mod lex;
mod parse;
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

    println!("Preprocessed: {:#?}", lexer);

    if let Some(ast) = parse::parse_ast(&mut lexer) {
        // codegen::codegen::ast_codegen(&ast);
    }
}
