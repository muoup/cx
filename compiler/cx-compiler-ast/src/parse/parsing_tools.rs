use cx_util::log_error;
use cx_data_ast::assert_token_matches;
use cx_data_ast::lex::token::{PunctuatorType, TokenKind};
use cx_data_ast::parse::parser::ParserData;

pub fn goto_statement_end(data: &mut ParserData) -> Option<()> {
    while let Some(token) = data.toks.next() {
        match &token.kind {
            TokenKind::Punctuator(PunctuatorType::Semicolon) =>
                return Some(()),

            TokenKind::Punctuator(PunctuatorType::OpenBrace) =>
                return goto_block_end(data.back()),

            _ => continue
        }
    }

    None
}

pub fn goto_block_end(data: &mut ParserData) -> Option<()> {
    assert_token_matches!(data.toks, TokenKind::Punctuator(PunctuatorType::OpenBrace));

    let mut brace_counter = 1;

    while let Some(token) = data.toks.next() {
        match &token.kind {
            TokenKind::Punctuator(PunctuatorType::OpenBrace) => {
                brace_counter += 1;
            }
            TokenKind::Punctuator(PunctuatorType::CloseBrace) => {
                brace_counter -= 1;
                if brace_counter == 0 {
                    return Some(());
                }
            }
            _ => {}
        }
    }

    None
}