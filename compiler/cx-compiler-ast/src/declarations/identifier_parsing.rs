use cx_data_ast::peek_next_kind;
use cx_data_lexer::{token::TokenKind, TokenIter};
use cx_util::identifier::CXIdent;

pub fn parse_intrinsic(tokens: &mut TokenIter) -> Option<CXIdent> {
    let mut ss = String::new();

    while let Some(TokenKind::Intrinsic(ident)) = peek_next_kind!(tokens) {
        ss.push_str(format!("{ident:?}").to_lowercase().as_str());
        tokens.next();
    }

    if ss.is_empty() {
        return None;
    }

    Some(CXIdent::from(ss))
}

pub fn parse_std_ident(tokens: &mut TokenIter) -> Option<CXIdent> {
    let TokenKind::Identifier(ident) = tokens.peek().cloned()?.kind else {
        return None;
    };

    tokens.next();

    Some(CXIdent::from(ident))
}
