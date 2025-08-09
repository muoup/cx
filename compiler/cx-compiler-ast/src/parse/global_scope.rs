use cx_data_ast::{assert_token_matches, try_next};
use cx_data_lexer::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, TokenKind};
use crate::parse::expression::{parse_expr, requires_semicolon};
use cx_data_ast::parse::ast::{CXExpr, CXExprKind, CXGlobalConstant, CXGlobalStmt, CXGlobalVariable};
use cx_data_ast::parse::parser::{ParserData, VisibilityMode};
use cx_data_ast::parse::type_mapping::contextualize_fn_prototype;
use cx_data_ast::preparse::pp_type::CXNaivePrototype;
use cx_util::{log_error, point_log_error, CXResult};
use crate::parse::template::parse_template;
use crate::preparse::preparser::{goto_statement_end, parse_std_ident};
use crate::preparse::typing::{parse_initializer, parse_params};

pub(crate) fn parse_global_stmt(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    match data.tokens.peek()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!")
        .kind {
        
        TokenKind::Keyword(KeywordType::Typedef) |
        TokenKind::Keyword(KeywordType::Import) => {
            goto_statement_end(&mut data.tokens);
            Some(None)
        },
        
        TokenKind::Punctuator(PunctuatorType::Semicolon) => {
            data.tokens.next();
            Some(None)
        },

        TokenKind::Keyword(KeywordType::Enum) => parse_enum_constants(data),
        TokenKind::Operator(OperatorType::Tilda) => parse_destructor(data),
        TokenKind::Specifier(_) => parse_access_mods(data),
        
        TokenKind::Keyword(KeywordType::Template) => parse_template(data),

        _ => parse_global_expr(data)
    }
}

pub(crate) fn parse_access_mods(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    assert_token_matches!(data.tokens, TokenKind::Specifier(specifier));

    match specifier {
        SpecifierType::Public => {
            data.visibility = VisibilityMode::Public;
            try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::Colon));
        },
        SpecifierType::Private => {
            data.visibility = VisibilityMode::Private;
            try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::Colon));
        },

        _ => todo!("parse_access_mods: {:#?}", specifier)
    };

    Some(None)
}

pub(crate) fn parse_destructor(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    assert_token_matches!(data.tokens, TokenKind::Operator(OperatorType::Tilda));
    assert_token_matches!(data.tokens, TokenKind::Identifier(name));
    
    let name = name.clone();
    let body = parse_body(data)?;

    Some(
        Some(
            CXGlobalStmt::DestructorDefinition {
                type_name: name,
                body: Box::new(body),
            }
        )
    )
}

pub(crate) fn parse_enum_constants(data: &mut ParserData) -> Option<Option<CXGlobalStmt>> {
    assert_token_matches!(data.tokens, TokenKind::Keyword(KeywordType::Enum));

    let Some(name) = parse_std_ident(&mut data.tokens) else {
        point_log_error!(data.tokens, "PARSER ERROR: Failed to parse enum name");
    };

    if !try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenBrace)) {
        return Some(None);
    }

    let mut counter = 0;

    loop {
        assert_token_matches!(data.tokens, TokenKind::Identifier(enum_name));
        let enum_name = enum_name.clone();
        
        if try_next!(data.tokens, TokenKind::Assignment(None)) {
            assert_token_matches!(data.tokens, TokenKind::IntLiteral(value));
            counter = *value as i32;
        }
        
        data.ast.global_variables.insert(
            enum_name.as_str().into(),
            CXGlobalVariable::GlobalConstant {
                anonymous: true,
                constant: CXGlobalConstant::Int(counter)
            }
        );

        counter += 1;

        if !try_next!(data.tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }
    
    assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseBrace));

    Some(None)
}

pub(crate) fn parse_global_expr(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    let Some((name, return_type)) = parse_initializer(&mut data.tokens) else {
        point_log_error!(data.tokens, "PARSER ERROR: Failed to parse initializer in global expression!");
    };
    
    let Some(name) = name else {
        goto_statement_end(&mut data.tokens);
        return Some(None);
    };
    
    if !data.tokens.has_next() {
        point_log_error!(data.tokens, "PARSER ERROR: Reached end of token stream when parsing global expression!");
    }

    match data.tokens.peek().unwrap().kind {
        TokenKind::Punctuator(PunctuatorType::OpenParen) => {
            let Some(result) = parse_params(&mut data.tokens) else {
                log_error!("PARSER ERROR: Failed to parse parameters in function declaration!");
            };

            let naive_prototype = CXNaivePrototype {
                return_type, name,
                params: result.params,
                var_args: result.var_args,
            };
            let prototype = contextualize_fn_prototype(&data.ast.type_map, &naive_prototype)
                .expect("CRITICAL: Failed to contextualize function prototype in global expression!");
            
            if try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::Semicolon)) {
                Some(Some(CXGlobalStmt::FunctionPrototype { prototype }))
            } else {
                let body = Box::new(parse_body(data)?);
                Some(Some(CXGlobalStmt::FunctionDefinition { prototype, body }))
            }
        },

        TokenKind::Punctuator(PunctuatorType::Semicolon)
        | TokenKind::Assignment(_) => todo!("Global variables"),
        
        _ => {
            goto_statement_end(&mut data.tokens);
            Some(None)
        },
    }
}

pub(crate) fn parse_body(data: &mut ParserData) -> Option<CXExpr> {
    if try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenBrace)) {
        let start_index = data.tokens.index - 1;
        let mut body = Vec::new();

        while !try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseBrace)) {
            let start_index = data.tokens.index;

            if let Some(stmt) = parse_expr(data) {
                if requires_semicolon(&stmt) {
                    assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::Semicolon));
                }

                body.push(stmt);
            } else {
                for i in start_index.. data.tokens.index {
                    eprintln!("Token: {:#?}", data.tokens.slice[i]);
                }

                log_error!("Failed to parse statement in body: {:#?}", data.tokens.peek());
            }
        }

        Some (
            CXExprKind::Block {
                exprs: body,
                value: None
            }.into_expr(start_index, data.tokens.index)
        )
    } else {
        let body = parse_expr(data)?;

        if requires_semicolon(&body) {
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::Semicolon));
        }

        Some(body)
    }
}