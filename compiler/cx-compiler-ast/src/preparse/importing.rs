use cx_data_ast::lex::token::{OperatorType, PunctuatorType, TokenKind};
use cx_data_ast::{assert_token_matches, keyword, punctuator, PreparseContents};
use cx_data_ast::parse::parser::{ParserData, TokenIter};
use cx_util::log_error;

pub(crate) fn parse_import(tokens: &mut TokenIter, contents: &mut PreparseContents) -> Option<()> {
    assert_token_matches!(tokens, keyword!(Import));

    let mut import_path = String::new();

    loop {
        let Some(tok) = tokens.next() else {
            log_error!("PARSER ERROR: Reached end of token stream when parsing import!");
        };

        match &tok.kind {
            TokenKind::Punctuator(PunctuatorType::Semicolon) => break,
            TokenKind::Operator(OperatorType::ScopeRes) => import_path.push('/'),
            TokenKind::Identifier(ident) => import_path.push_str(ident),

            _ => log_error!("PARSER ERROR: Reached invalid token in import path: {:?}", tok)
        }
    };
    
    contents.imports.push(import_path);
    Some(())
}