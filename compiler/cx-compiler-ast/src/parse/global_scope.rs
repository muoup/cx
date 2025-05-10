use cx_data_ast::{assert_token_matches, try_next};
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, Token};
use crate::parse::expression::{parse_expr, requires_semicolon};
use cx_data_ast::parse::ast::{CXExpr, CXFunctionPrototype, CXGlobalStmt, CXParameter, CXAST};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::parser::{ParserData, VisibilityMode};
use cx_data_ast::parse::value_type::{CXTypeUnion, CXValType};
use crate::parse::typing::{parse_initializer, parse_plain_typedef};
use cx_util::log_error;
use crate::parse::parsing_tools::goto_statement_end;

pub(crate) fn parse_global_stmt(data: &mut ParserData, ast: &mut CXAST) -> Option<()> {
    match data.toks.peek()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!") {

        Token::Keyword(KeywordType::Import) => parse_import(data, ast),

        Token::Keyword(KeywordType::Typedef) |
        Token::Keyword(KeywordType::Struct) |
        Token::Keyword(KeywordType::Enum) |
        Token::Keyword(KeywordType::Union) => goto_statement_end(data),

        Token::Specifier(_) => parse_specifier(data, ast),

        _ => parse_global_expr(data, ast)
    }
}

pub(crate) fn parse_specifier(data: &mut ParserData, _: &mut CXAST) -> Option<()> {
    assert_token_matches!(data, Token::Specifier(specifier));

    match specifier {
        SpecifierType::Public => {
            data.visibility = VisibilityMode::Public;
            try_next!(data, Token::Punctuator(PunctuatorType::Colon));
        },
        SpecifierType::Private => {
            data.visibility = VisibilityMode::Private;
            try_next!(data, Token::Punctuator(PunctuatorType::Colon));
        },

        _ => unimplemented!("parse_specifier: {:#?}", specifier)
    };

    Some(())
}

pub(crate) fn parse_import(data: &mut ParserData, ast: &mut CXAST) -> Option<()> {
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

    ast.imports.push(import_path);
    Some(())
}

fn handle_member_this(class_name: &str, params: &mut Vec<CXParameter>) {
    let Some(first_param) = params.first_mut() else {
        return;
    };

    let CXTypeUnion::Identifier(ident) = &first_param.type_.internal_type else {
        return;
    };

    if matches!(ident.as_str(), "this") {
        let take_param = std::mem::replace(first_param, CXParameter { name: None, type_: CXValType::unit() });

        *first_param = CXParameter {
            name: take_param.name,
            type_: CXValType::new(
                0,
                CXTypeUnion::PointerTo(
                    Box::new(take_param.type_)
                )
            )
        };
    }
}

pub(crate) fn parse_global_expr(data: &mut ParserData, ast: &mut CXAST) -> Option<()> {
    let Some((name, return_type)) = parse_initializer(data) else {
        log_error!("PARSER ERROR: Failed to parse initializer in global expression!");
    };

    let Some(name) = name else {
        log_error!("PARSER ERROR: Identifier expected, found none!");
    };

    match data.toks.peek() {
        Some(Token::Punctuator(PunctuatorType::OpenParen)) => {
            let Some(mut params) = parse_params(data) else {
                log_error!("PARSER ERROR: Failed to parse parameters in function declaration!");
            };

            let prototype = CXFunctionPrototype {
                return_type, name, parameters: params
            };

            if try_next!(data, Token::Punctuator(PunctuatorType::Semicolon)) {
                ast.global_stmts.push(CXGlobalStmt::FunctionForward { prototype });
            } else {
                let body = parse_body(data)?;
                ast.global_stmts.push(CXGlobalStmt::FunctionDefinition { prototype, body })
            }

            Some(())
        },

        Some(Token::Punctuator(PunctuatorType::Semicolon))
        | Some(Token::Assignment(_)) => todo!("Global variables"),

        _ => log_error!("PARSER ERROR: Expected a function declaration or variable assignment after initializer {:?} {:?}! Found token: {:?}", return_type, name, data.toks.peek()),
    }
}

pub(crate) fn parse_params(data: &mut ParserData) -> Option<Vec<CXParameter>> {
    assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenParen));

    let mut params = Vec::new();

    while !try_next!(data, Token::Punctuator(PunctuatorType::CloseParen)) {
        if let Some((name, type_)) = parse_initializer(data) {
            let name = name.map(|name| name);

            params.push(CXParameter { name, type_ });
        } else {
            log_error!("Failed to parse parameter in function call: {:#?}", data.toks.peek());
        }

        if !try_next!(data, Token::Operator(OperatorType::Comma)) { assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));
            break;
        }
    }

    Some(params)
}

pub(crate) fn parse_body(data: &mut ParserData) -> Option<CXExpr> {
    if try_next!(data, Token::Punctuator(PunctuatorType::OpenBrace)) {
        let mut body = Vec::new();

        while !try_next!(data, Token::Punctuator(PunctuatorType::CloseBrace)) {
            let start_index = data.toks.index;

            if let Some(stmt) = parse_expr(data) {
                if requires_semicolon(&stmt) {
                    assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));
                }

                body.push(stmt);
            } else {
                for i in start_index.. data.toks.index {
                    eprintln!("Token: {:#?}", data.toks.slice[i]);
                }

                log_error!("Failed to parse statement in body: {:#?}", data.toks.peek());
            }
        }

        Some (
            CXExpr::Block {
                exprs: body,
                value: None
            }
        )
    } else {
        let body = parse_expr(data)?;
        assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

        Some(body)
    }
}