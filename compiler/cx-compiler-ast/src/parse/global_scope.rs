use cx_data_ast::{assert_token_matches, next_kind, try_next};
use cx_data_lexer::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, TokenKind};
use crate::parse::expression::{parse_expr, requires_semicolon};
use cx_data_ast::parse::ast::{CXExpr, CXExprKind, CXGlobalStmt};
use cx_util::identifier::CXIdent;
use cx_data_ast::parse::parser::{ParserData, VisibilityMode};
use cx_data_ast::preparse::CXNaiveFnIdent;
use cx_data_ast::preparse::naive_types::{CXNaiveParameter, CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, PredeclarationType};
use cx_data_lexer::{identifier, keyword, operator, punctuator, specifier};
use cx_util::{log_error, point_log_error, CXResult};
use crate::parse::template::parse_template;
use crate::preparse::preparser::{goto_statement_end, parse_std_ident, try_function_parse};
use crate::preparse::typing::parse_initializer;

pub(crate) fn parse_global_stmt(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    match data.tokens.peek()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!")
        .kind {
        
        keyword!(Typedef) |
        keyword!(Import) => {
            goto_statement_end(&mut data.tokens);
            Some(None)
        },
        
        punctuator!(Semicolon) => {
            data.tokens.next();
            Some(None)
        },
        
        keyword!(Enum) => parse_enum_constants(data),
        operator!(Tilda) => parse_destructor(data),
        specifier!() => parse_access_mods(data),
        
        keyword!(Template) => parse_template(data),

        _ => parse_global_expr(data)
    }
}

pub(crate) fn parse_access_mods(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    assert_token_matches!(data.tokens, TokenKind::Specifier(specifier));

    match specifier {
        SpecifierType::Public => data.visibility = VisibilityMode::Public,
        SpecifierType::Private => data.visibility = VisibilityMode::Private,

        _ => point_log_error!(data.tokens, "PARSER ERROR: Unexpected specifier in global scope"),
    };
    
    try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::Colon));

    Some(None)
}

pub(crate) fn parse_destructor(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    assert_token_matches!(data.tokens, TokenKind::Operator(OperatorType::Tilda));
    let Some((None, _type)) = parse_initializer(&mut data.tokens) else {
        point_log_error!(data.tokens, "PARSER ERROR: Failed to parse type in destructor definition!");
    };
    assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::OpenParen));
    assert_token_matches!(data.tokens, identifier!(this));
    assert_eq!(this, "this");
    assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseParen));

    let body = parse_body(data)?;

    Some(
        Some(
            CXGlobalStmt::DestructorDefinition {
                _type, body: Box::new(body)
            }
        )
    )
}

pub(crate) fn destructor_prototype(_type: CXNaiveType) -> CXNaivePrototype {
    let Some(name) = _type.get_name().cloned() else {
        unreachable!("CRITICAL: destructor_prototype() called with unnamed type!");
    };

    CXNaivePrototype {
        name: CXNaiveFnIdent::Destructor(name),

        return_type: CXNaiveTypeKind::Identifier {
            name: CXIdent::from("void"),
            predeclaration: PredeclarationType::None
        }.to_type(),
        params: vec![
            CXNaiveParameter {
                name: Some(CXIdent::from("this")),
                _type: _type.pointer_to(true, 0),
            }
        ],
        var_args: false,
        this_param: true,
    }
}

pub(crate) fn parse_enum_constants(data: &mut ParserData) -> Option<Option<CXGlobalStmt>> {
    assert_token_matches!(data.tokens, TokenKind::Keyword(KeywordType::Enum));

    let _ = parse_std_ident(&mut data.tokens);

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
        
        data.ast.enum_constants.push((enum_name.as_str().into(), counter as i64));

        counter += 1;

        if !try_next!(data.tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }
    
    assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::CloseBrace));

    Some(None)
}

fn parse_fn_merge(data: &mut ParserData, prototype: CXNaivePrototype) -> CXResult<Option<CXGlobalStmt>> {
    if try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::Semicolon)) {
        Some(Some(CXGlobalStmt::FunctionPrototype { prototype }))
    } else {
        let body = Box::new(parse_body(data)?);
        Some(Some(CXGlobalStmt::FunctionDefinition { prototype, body }))
    }
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
    
    if let Some(func) = try_function_parse(&mut data.tokens, return_type.clone(), name.clone())? {
        return parse_fn_merge(data, func);
    }

    match next_kind!(data.tokens) {
        Some(TokenKind::Assignment(_)) => {
            let initial_value = parse_expr(data)?;
            assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::Semicolon));

            Some(
                Some(
                    CXGlobalStmt::GlobalVariable {
                        name,
                        type_: return_type,
                        initializer: Some(initial_value),
                    }
                )
            )
        },

        Some(TokenKind::Punctuator(PunctuatorType::Semicolon)) => {
            Some(
                Some(
                    CXGlobalStmt::GlobalVariable {
                        name,
                        type_: return_type,
                        initializer: None,
                    }
                )
            )
        },

        _ => point_log_error!(data.tokens, "PARSER ERROR: Unexpected token in global expression: {:#?}", data.tokens.peek()),
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