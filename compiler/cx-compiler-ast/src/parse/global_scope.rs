use cx_data_ast::{assert_token_matches, try_next};
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, TokenKind};
use crate::parse::expression::{parse_expr, requires_semicolon};
use cx_data_ast::parse::ast::{CXExpr, CXExprKind, CXFunctionPrototype, CXGlobalStmt, CXParameter, CXAST};
use cx_data_ast::parse::parser::{ParserData, VisibilityMode};
use crate::parse::typing::parse_initializer;
use cx_util::{log_error, point_log_error};
use crate::parse::parsing_tools::goto_statement_end;

pub(crate) fn parse_global_stmt(data: &mut ParserData, ast: &mut CXAST) -> Option<()> {
    match data.toks.peek()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!")
        .kind {

        TokenKind::Keyword(KeywordType::Typedef) |
        TokenKind::Keyword(KeywordType::Import) => goto_statement_end(data),
        
        TokenKind::Punctuator(PunctuatorType::Semicolon) => {
            data.toks.next();
            Some(())
        },
        TokenKind::Specifier(_) => parse_access_mods(data, ast),

        _ => parse_global_expr(data, ast)
    }
}

pub(crate) fn parse_access_mods(data: &mut ParserData, _: &mut CXAST) -> Option<()> {
    assert_token_matches!(data, TokenKind::Specifier(specifier));

    match specifier {
        SpecifierType::Public => {
            data.visibility = VisibilityMode::Public;
            try_next!(data, TokenKind::Punctuator(PunctuatorType::Colon));
        },
        SpecifierType::Private => {
            data.visibility = VisibilityMode::Private;
            try_next!(data, TokenKind::Punctuator(PunctuatorType::Colon));
        },

        _ => unimplemented!("parse_access_mods: {:#?}", specifier)
    };

    Some(())
}

pub(crate) fn parse_import(data: &mut ParserData) -> Option<String> {
    assert_token_matches!(data, TokenKind::Keyword(KeywordType::Import));

    let mut import_path = String::new();

    loop {
        let Some(tok) = data.toks.next() else {
            log_error!("PARSER ERROR: Reached end of token stream when parsing import!");
        };

        match &tok.kind {
            TokenKind::Punctuator(PunctuatorType::Semicolon) => break,
            TokenKind::Operator(OperatorType::ScopeRes) => import_path.push('/'),
            TokenKind::Identifier(ident) => import_path.push_str(ident),

            _ => {
                log_error!("PARSER ERROR: Reached invalid token in import path: {:?}", tok);
            }
        }
    };
    
    Some(import_path)
}

pub(crate) fn parse_global_expr(data: &mut ParserData, ast: &mut CXAST) -> Option<()> {
    let Some((name, return_type)) = parse_initializer(data) else {
        point_log_error!(data, "PARSER ERROR: Failed to parse initializer in global expression!");
    };
    
    let Some(name) = name else {
        goto_statement_end(data);
        return Some(());
    };
    
    if !data.toks.has_next() {
        point_log_error!(data, "PARSER ERROR: Reached end of token stream when parsing global expression!");
    }

    match data.toks.peek().unwrap().kind {
        TokenKind::Punctuator(PunctuatorType::OpenParen) => {
            let Some(result) = parse_params(data) else {
                log_error!("PARSER ERROR: Failed to parse parameters in function declaration!");
            };

            let prototype = CXFunctionPrototype {
                return_type, name,
                params: result.params,
                var_args: result.var_args,
            };
            
            ast.function_map.insert(prototype.name.as_string(), prototype.clone());
            
            if data.visibility == VisibilityMode::Public {
                ast.public_functions.push(prototype.name.as_string());
            }

            if !try_next!(data, TokenKind::Punctuator(PunctuatorType::Semicolon)) {
                let body = Box::new(parse_body(data)?);
                ast.global_stmts.push(CXGlobalStmt::FunctionDefinition { prototype, body })
            }

            Some(())
        },

        TokenKind::Punctuator(PunctuatorType::Semicolon)
        | TokenKind::Assignment(_) => todo!("Global variables"),

        _ => {
            goto_statement_end(data);
            Some(())
        },
    }
}

pub(crate) struct ParseParamsResult {
    pub(crate) params: Vec<CXParameter>,
    pub(crate) var_args: bool,
}

pub(crate) fn parse_params(data: &mut ParserData) -> Option<ParseParamsResult> {
    assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::OpenParen));

    let mut params = Vec::new();

    while !try_next!(data, TokenKind::Punctuator(PunctuatorType::CloseParen)) {
        if try_next!(data, TokenKind::Punctuator(PunctuatorType::Ellipsis)) {
            assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::CloseParen));
            return Some(ParseParamsResult { params, var_args: true });
        }

        if let Some((name, type_)) = parse_initializer(data) {
            let name = name;

            params.push(CXParameter { name, type_ });
        } else {
            log_error!("Failed to parse parameter in function call: {:#?}", data.toks.peek());
        }

        if !try_next!(data, TokenKind::Operator(OperatorType::Comma)) { assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::CloseParen));
            break;
        }
    }

    Some(ParseParamsResult { params, var_args: false })
}

pub(crate) fn parse_body(data: &mut ParserData) -> Option<CXExpr> {
    if try_next!(data, TokenKind::Punctuator(PunctuatorType::OpenBrace)) {
        let start_index = data.toks.index - 1;
        let mut body = Vec::new();

        while !try_next!(data, TokenKind::Punctuator(PunctuatorType::CloseBrace)) {
            let start_index = data.toks.index;

            if let Some(stmt) = parse_expr(data) {
                if requires_semicolon(&stmt) {
                    assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::Semicolon));
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
            CXExprKind::Block {
                exprs: body,
                value: None
            }.into_expr(start_index, data.toks.index)
        )
    } else {
        let body = parse_expr(data)?;

        if requires_semicolon(&body) {
            assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::Semicolon));
        }

        Some(body)
    }
}