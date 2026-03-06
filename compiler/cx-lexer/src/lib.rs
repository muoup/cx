use cx_tokens::token::Token;

pub(crate) mod line_lexer;
pub(crate) mod preprocessor;
pub(crate) mod unified_lexer;

pub fn lex(source: &str) -> Option<Vec<Token>> {
    let mut lexer = crate::unified_lexer::Lexer::new(source);
    lexer.lex_source();

    Some(lexer.tokens)
}
