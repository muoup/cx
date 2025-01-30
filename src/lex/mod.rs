mod lexer;
pub mod token;

pub fn generate_tokens(source: &str) -> Vec<token::Token> {
    let mut lexer = lexer::Lexer::new(source);
    lexer.generate_tokens();
    lexer.tokens
}