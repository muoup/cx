use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::{assert_token_matches, log_error, try_consume_token};
use crate::parse::expression::parse_identifier;
use crate::parse::parser::ParserData;
use crate::parse::pass_unverified::{UVExpr, UVGlobalStmt};
use crate::parse::pass_unverified::expression::{parse_expr, requires_semicolon};

pub(crate) fn parse_global_stmt(data: &mut ParserData) -> Option<UVGlobalStmt> {
    match data.toks.next()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!") {
        Token::Keyword(KeywordType::Import) => parse_import(data),
        _ => {
            data.toks.back();
            parse_global_expr(data)
        }
    }
}

pub(crate) fn parse_import(data: &mut ParserData) -> Option<UVGlobalStmt> {
    let mut import_path = String::new();

    loop {
        let Some(tok) = data.toks.next() else {
            eprintln!("PARSER ERROR: Reached end of token stream when parsing import!");
            return None;
        };

        match tok {
            Token::Punctuator(PunctuatorType::Semicolon) => break,
            Token::Operator(OperatorType::ScopeRes) => import_path.push('/'),
            Token::Identifier(ident) => import_path.push_str(&ident),

            _ => {
                eprintln!("PARSER ERROR: Reached invalid token in import path: {:?}", tok);
                return None;
            }
        }
    };

    Some(UVGlobalStmt::Import(import_path))
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

pub(crate) fn parse_body(data: &mut ParserData) -> Option<UVExpr> {
    if try_consume_token!(data, Token::Punctuator(PunctuatorType::OpenBrace)) {
        let mut body = Vec::new();

        while !try_consume_token!(data, Token::Punctuator(PunctuatorType::CloseBrace)) {
            if let Some(stmt) = parse_expr(data) {
                if requires_semicolon(&stmt) {
                    assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));
                }

                body.push(stmt);
            } else {
                log_error!("Failed to parse statement in body: {:#?}", data.toks.peek());
            }
        }

        Some(UVExpr::ExprChain(body))
    } else {
        let body = parse_expr(data)?;
        assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

        Some(body)
    }
}