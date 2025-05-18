use cx_util::log_error;
use cx_data_ast::assert_token_matches;
use cx_data_ast::lex::token::{PunctuatorType, Token};
use cx_data_ast::parse::parser::ParserData;

pub fn goto_statement_end(data: &mut ParserData) -> Option<()> {
    while let Some(token) = data.toks.next() {
        match token {
            Token::Punctuator(PunctuatorType::Semicolon) =>
                return Some(()),

            Token::Punctuator(PunctuatorType::OpenBrace) =>
                return goto_block_end(data.back()),

            _ => continue
        }
    }

    None
}

pub fn goto_block_end(data: &mut ParserData) -> Option<()> {
    assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenBrace));

    let mut brace_counter = 1;

    while let Some(token) = data.toks.next() {
        match token {
            Token::Punctuator(PunctuatorType::OpenBrace) => {
                brace_counter += 1;
            }
            Token::Punctuator(PunctuatorType::CloseBrace) => {
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