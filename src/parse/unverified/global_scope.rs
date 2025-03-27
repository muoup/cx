use crate::lex::token::{KeywordType, PunctuatorType, Token};
use crate::{assert_token_matches, log_error, try_consume_token};
use crate::parse::expression::parse_identifier;
use crate::parse::parser::ParserData;
use crate::parse::unverified::{UVExpr, UVGlobalStmt};
use crate::parse::unverified::expression::parse_expr;

pub(crate) fn parse_global_stmt(data: &mut ParserData) -> Option<UVGlobalStmt> {
    match data.toks.peek()? {
        _ => parse_global_expr(data)
    }
}

pub(crate) fn parse_global_expr(data: &mut ParserData) -> Option<UVGlobalStmt> {
    let Some(identifier) = parse_identifier(data) else {
        log_error!("Could not parse global expression starting with {:#?}", data.toks.peek());
    };

    let Some(expr) = parse_expr(data) else {
        log_error!("Failed to parse expression for global statement: {:#?}", data.toks.peek());
    };

    if try_consume_token!(data, Token::Punctuator(PunctuatorType::Semicolon)) {
        return Some(UVGlobalStmt::SingleExpression {
            expression: UVExpr::Compound {
                left: Box::new(UVExpr::Identifier(identifier)),
                right: Box::new(expr)
            }
        });
    }

    let body = parse_body(data)?;

    Some(
        UVGlobalStmt::BodiedExpression {
            header:
                UVExpr::Compound {
                    left: Box::new(UVExpr::Identifier(identifier)),
                    right: Box::new(expr)
                },
            body
        }
    )
}

pub(crate) fn parse_body(data: &mut ParserData) -> Option<Vec<UVExpr>> {
    if try_consume_token!(data, Token::Punctuator(PunctuatorType::OpenBrace)) {
        let mut body = Vec::new();

        while !try_consume_token!(data, Token::Punctuator(PunctuatorType::CloseBrace)) {
            if let Some(stmt) = parse_expr(data) {
                body.push(stmt);
                assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));
            } else {
                log_error!("Failed to parse statement in body: {:#?}", data.toks.peek());
            }
        }

        Some(body)
    } else {
        let body = parse_expr(data)?;
        assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

        Some(vec![body])
    }
}