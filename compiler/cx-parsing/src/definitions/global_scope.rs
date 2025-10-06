use cx_lexer_data::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, TokenKind};
use cx_lexer_data::{keyword, operator, punctuator, specifier};
use cx_parsing_data::parse::ast::{CXExpr, CXExprKind, CXGlobalStmt};
use cx_parsing_data::parse::parser::{ParserData, VisibilityMode};
use cx_parsing_data::preparse::naive_types::{
    CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, PredeclarationType,
};
use cx_parsing_data::preparse::{FunctionTypeIdent, NaiveFnIdent};
use cx_parsing_data::{assert_token_matches, next_kind, try_next};
use cx_util::identifier::CXIdent;
use cx_util::CXResult;

use crate::declarations::data_parsing::parse_std_ident;
use crate::declarations::function_parsing::{parse_destructor_prototype, try_function_parse};
use crate::declarations::type_parsing::parse_initializer;
use crate::declarations::FunctionDeclaration;
use crate::definitions::expression::{expression_requires_semicolon, parse_expr};
use crate::definitions::template::{note_templated_types, unnote_templated_types};

pub(crate) fn parse_global_stmt(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    match data
        .tokens
        .peek()
        .expect("CRITICAL: parse_global_stmt() should not be called with no remaining tokens!")
        .kind
    {
        keyword!(Typedef) | keyword!(Import) => {
            data.tokens.goto_statement_end();
            Some(None)
        }

        punctuator!(Semicolon) => {
            data.tokens.next();
            Some(None)
        }

        keyword!(Enum) => parse_enum_constants(data),
        specifier!() => parse_access_mods(data),

        operator!(Tilda) => {
            let destructor = parse_destructor_prototype(&mut data.tokens)?;
            parse_fn_merge(data, destructor)
        }

        _ => parse_global_expr(data),
    }
}

pub(crate) fn parse_access_mods(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    assert_token_matches!(data.tokens, TokenKind::Specifier(specifier));

    match specifier {
        SpecifierType::Public => data.visibility = VisibilityMode::Public,
        SpecifierType::Private => data.visibility = VisibilityMode::Private,

        _ => log_parse_error!(data, "Unexpected specifier in global scope"),
    };

    try_next!(data.tokens, TokenKind::Punctuator(PunctuatorType::Colon));

    Some(None)
}

pub(crate) fn destructor_prototype(_type: CXNaiveType) -> CXNaivePrototype {
    CXNaivePrototype {
        name: NaiveFnIdent::Destructor(FunctionTypeIdent::from_type(&_type).unwrap()),

        return_type: CXNaiveTypeKind::Identifier {
            name: CXIdent::from("void"),
            predeclaration: PredeclarationType::None,
        }
        .to_type(),
        params: vec![],
        var_args: false,
        this_param: true,
    }
}

pub(crate) fn parse_enum_constants(data: &mut ParserData) -> Option<Option<CXGlobalStmt>> {
    assert_token_matches!(data.tokens, TokenKind::Keyword(KeywordType::Enum));

    let _ = parse_std_ident(&mut data.tokens);

    if !try_next!(
        data.tokens,
        TokenKind::Punctuator(PunctuatorType::OpenBrace)
    ) {
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

        data.ast
            .enum_constants
            .push((enum_name.as_str().into(), counter as i64));

        counter += 1;

        if !try_next!(data.tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    assert_token_matches!(
        data.tokens,
        TokenKind::Punctuator(PunctuatorType::CloseBrace)
    );

    Some(None)
}

fn parse_fn_merge(
    data: &mut ParserData,
    prototype: FunctionDeclaration,
) -> CXResult<Option<CXGlobalStmt>> {
    if try_next!(
        data.tokens,
        TokenKind::Punctuator(PunctuatorType::Semicolon)
    ) {
        if prototype.template_prototype.is_some() {
            log_parse_error!(data, "Templated functions must be defined in place.");
        }

        Some(Some(CXGlobalStmt::FunctionPrototype {
            prototype: prototype.prototype,
        }))
    } else {
        match &prototype.template_prototype {
            Some(template_prototype) => {
                note_templated_types(data, template_prototype);
                let body = parse_body(data)?;
                unnote_templated_types(data, template_prototype);

                Some(Some(CXGlobalStmt::TemplatedFunction {
                    prototype: prototype.prototype,
                    body: Box::new(body),
                }))
            }

            None => {
                let body = parse_body(data)?;

                Some(Some(CXGlobalStmt::FunctionDefinition {
                    prototype: prototype.prototype,
                    body: Box::new(body),
                }))
            }
        }
    }
}

pub(crate) fn parse_global_expr(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    let Some((name, return_type)) = parse_initializer(&mut data.tokens) else {
        log_parse_error!(data, "Failed to parse initializer in global expression!");
    };

    let Some(name) = name else {
        data.tokens.goto_statement_end();
        return Some(None);
    };

    if !data.tokens.has_next() {
        log_parse_error!(
            data,
            "Reached end of token stream when parsing global expression!"
        );
    }

    if let Some(func) = try_function_parse(&mut data.tokens, return_type.clone(), name.clone())? {
        return parse_fn_merge(data, func);
    }

    match next_kind!(data.tokens) {
        Some(TokenKind::Assignment(_)) => {
            let initial_value = parse_expr(data)?;
            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::Semicolon)
            );

            Some(Some(CXGlobalStmt::GlobalVariable {
                name,
                type_: return_type,
                initializer: Some(initial_value),
            }))
        }

        Some(TokenKind::Punctuator(PunctuatorType::Semicolon)) => {
            Some(Some(CXGlobalStmt::GlobalVariable {
                name,
                type_: return_type,
                initializer: None,
            }))
        }

        _ => log_parse_error!(
            data,
            "Unexpected token in global expression: {:#?}",
            data.tokens.peek()
        ),
    }
}

pub(crate) fn parse_body(data: &mut ParserData) -> Option<CXExpr> {
    if try_next!(data.tokens, punctuator!(OpenBrace)) {
        let start_index = data.tokens.index - 1;
        let mut body = Vec::new();

        while !try_next!(data.tokens, punctuator!(CloseBrace)) {
            let Some(stmt) = parse_expr(data) else {
                log_parse_error!(
                    data,
                    "Failed to parse statement in body: {:#?}",
                    data.tokens.peek()
                );
            };

            if expression_requires_semicolon(&stmt) {
                assert_token_matches!(data.tokens, punctuator!(Semicolon));
            }

            body.push(stmt);
        }

        Some(CXExprKind::Block { exprs: body }.into_expr(start_index, data.tokens.index))
    } else {
        let body = parse_expr(data)?;

        if expression_requires_semicolon(&body) {
            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::Semicolon)
            );
        }

        Some(body)
    }
}
