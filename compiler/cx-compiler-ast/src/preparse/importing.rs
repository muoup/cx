use cx_data_lexer::token::{OperatorType, PunctuatorType, TokenKind};
use cx_data_ast::assert_token_matches;
use cx_data_lexer::{keyword, TokenIter};
use cx_util::log_error;
use crate::preparse::preparser::PreparseResult;

pub(crate) fn parse_import(tokens: &mut TokenIter) -> Option<PreparseResult> {
    assert_token_matches!(tokens, keyword!(Import));

    let mut import_path = String::new();

    loop {
        let Some(tok) = tokens.next() else {
            log_error!("Reached end of token stream when parsing import!");
        };

        match &tok.kind {
            TokenKind::Punctuator(PunctuatorType::Semicolon) => break,
            TokenKind::Operator(OperatorType::ScopeRes) => import_path.push('/'),
            TokenKind::Identifier(ident) => import_path.push_str(ident),

            _ => log_error!("Reached invalid token in import path: {:?}", tok)
        }
    };
    
    Some(PreparseResult::Import(import_path))
}