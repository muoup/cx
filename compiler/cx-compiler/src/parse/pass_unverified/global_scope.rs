use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, Token};
use crate::{assert_token_matches, log_error, try_next};
use crate::parse::parser::{ParserData, VisibilityMode};
use crate::parse::pass_unverified::{UVExpr, UVGlobalStmt};
use crate::parse::pass_unverified::expression::{parse_expr, parse_identifier, requires_semicolon};
use crate::parse::pass_unverified::typing::{parse_initializer, parse_plain_typedef, parse_type};
use crate::parse::value_type::CXValType;

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
    let Some((name, val_type)) = parse_initializer(data) else {
        log_error!("PARSER ERROR: Failed to parse initializer in global expression!");
    };

    match data.toks.peek() {
        Some(Token::Punctuator(PunctuatorType::OpenParen)) => {
            let Some(params) = parse_params(data) else {
                log_error!("PARSER ERROR: Failed to parse parameters in function declaration!");
            };

            if try_next!(data, Token::Punctuator(PunctuatorType::Semicolon)) {
                return Some(UVGlobalStmt::Function {
                    name,
                    return_type: val_type,
                    params,
                    body: None
                });
            }

            let body = parse_body(data);

            Some(UVGlobalStmt::Function {
                name,
                return_type: val_type,
                params,
                body
            })
        },

        Some(Token::Punctuator(PunctuatorType::Semicolon))
        | Some(Token::Assignment(_)) => todo!("Global variables"),

        _ => log_error!("PARSER ERROR: Expected a function declaration or variable assignment after initializer! Found token: {:?}", data.toks.peek()),
    }
}

pub(crate) fn parse_params(data: &mut ParserData) -> Option<Vec<(String, CXValType)>> {
    assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenParen));

    let mut params = Vec::new();

    while !try_next!(data, Token::Punctuator(PunctuatorType::CloseParen)) {
        if let Some((name, type_)) = parse_initializer(data) {
            params.push((name, type_));
        } else {
            log_error!("Failed to parse parameter in function call: {:#?}", data.toks.peek());
        }

        if !try_next!(data, Token::Operator(OperatorType::Comma)) {
            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));
            break;
        }
    }

    Some(params)
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