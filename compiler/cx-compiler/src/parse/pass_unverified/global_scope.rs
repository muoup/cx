use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, Token};
use crate::{assert_token_matches, log_error, try_next};
use crate::parse::parser::{ParserData, VisibilityMode};
use crate::parse::pass_unverified::{UVExpr, UVGlobalStmt};
use crate::parse::pass_unverified::expression::{parse_expr, parse_identifier, requires_semicolon};
use crate::parse::pass_unverified::typing::parse_plain_typedef;

pub(crate) fn parse_global_stmt(data: &mut ParserData) -> Option<UVGlobalStmt> {
    match data.toks.peek()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!") {

        Token::Keyword(KeywordType::Import) => parse_import(data),

        Token::Keyword(KeywordType::Struct) |
        Token::Keyword(KeywordType::Enum) |
        Token::Keyword(KeywordType::Union) => parse_plain_typedef(data),

        Token::Specifier(_) => parse_specifier(data),

        _ => parse_global_expr(data)
    }
}

pub(crate) fn parse_specifier(data: &mut ParserData) -> Option<UVGlobalStmt> {
    assert_token_matches!(data, Token::Specifier(specifier));

    match specifier {
        SpecifierType::Public => {
            data.visibility = VisibilityMode::Public;
            try_next!(data, Token::Punctuator(PunctuatorType::Colon));
            Some(UVGlobalStmt::HandledInternally)
        },
        SpecifierType::Private => {
            data.visibility = VisibilityMode::Private;
            try_next!(data, Token::Punctuator(PunctuatorType::Colon));
            Some(UVGlobalStmt::HandledInternally)
        },

        _ => unimplemented!("parse_specifier: {:#?}", specifier)
    }
}

pub(crate) fn parse_import(data: &mut ParserData) -> Option<UVGlobalStmt> {
    assert_token_matches!(data, Token::Keyword(KeywordType::Import));

    let mut import_path = String::new();

    loop {
        let Some(tok) = data.toks.next() else {
            log_error!("PARSER ERROR: Reached end of token stream when parsing import!");
        };

        match tok {
            Token::Punctuator(PunctuatorType::Semicolon) => break,
            Token::Operator(OperatorType::ScopeRes) => import_path.push('/'),
            Token::Identifier(ident) => import_path.push_str(&ident),

            _ => {
                log_error!("PARSER ERROR: Reached invalid token in import path: {:?}", tok);
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

    if try_next!(data, Token::Punctuator(PunctuatorType::Semicolon)) {
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
    if try_next!(data, Token::Punctuator(PunctuatorType::OpenBrace)) {
        let mut body = Vec::new();

        while !try_next!(data, Token::Punctuator(PunctuatorType::CloseBrace)) {
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