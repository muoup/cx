use cx_data_ast::lex::token;

mod lexer;

pub fn lex(source: &str) -> Vec<token::Token> {
    let mut lexer = lexer::Lexer::new(source);
    lexer.generate_tokens();
    lexer.tokens
}