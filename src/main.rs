use crate::parse::ast::Node;

mod lex;
mod parse;

fn main() {
    let source = "int main() { return 0; }";
    let mut lexer = lex::lexer::Lexer::new(source);
    lexer.generate_tokens();

    for tok in &lexer.tokens {
        println!("{:?}", tok);
    }

    let ast = parse::parser::parse_ast(&lexer.tokens);

    if let Some(ast) = ast {
        ast.root.print(0);
    }
}
