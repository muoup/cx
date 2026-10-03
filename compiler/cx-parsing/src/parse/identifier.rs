use cx_log::catalogue::parse::*;
use cx_log::CXResult;
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_tokens::{operator, token::TokenKind, TokenIter};
use cx_util::identifier::CXIdent;

use crate::{log::parse_point_error, next_kind, try_next};

pub(crate) fn try_parse_qualified_name(tokens: &mut TokenIter) -> CXResult<Option<QualifiedName>> {
    if !matches!(
        tokens.peek().map(|token| &token.kind),
        Some(TokenKind::Identifier(_))
    ) {
        return Ok(None);
    };

    let mut segments = Vec::new();

    loop {
        let TokenKind::Identifier(ident) = next_kind!(tokens)? else {
            return parse_point_error(tokens, &EXPECTED_SYNTAX, ("a qualified identifier".into(), None));
        };

        segments.push(CXIdent::new(ident.clone()));

        if !try_next!(tokens, operator!(ScopeRes)) {
            break;
        }
    }

    let ident = segments
        .pop()
        .expect("identifier parser should have at least one segment");
    Ok(Some(QualifiedName::new(
        NamespacePath::new(segments),
        ident,
    )))
}
