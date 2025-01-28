use std::env;
use crate::parse::ast::Node;

mod lex;
mod parse;

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let Some(file_name) = args.get(1) else {
        println!("Usage: {} <file>", args[0]);
        return;
    };

    let source = std::fs::read_to_string(file_name).unwrap();

    let mut lexer = lex::lexer::Lexer::new(source.as_str());
    lexer.generate_tokens();

    for tok in &lexer.tokens {
        println!("{:?}", tok);
    }

    let ast = parse::parser::parse_ast(&lexer.tokens);

    if let Some(ast) = ast {
        ast.root.print(0);
    }
}
