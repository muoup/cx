use cx_data_lexer::token::Token;

pub(crate) mod unified_lexer;
pub(crate) mod line_lexer;
pub(crate) mod preprocessor;

pub fn lex(source: &str) -> Option<Vec<Token>> {
    let mut lexer = crate::unified_lexer::Lexer::new(source);
    lexer.lex_source(source)
}