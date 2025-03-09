use std::env::args;
use crate::{assert_token_matches, log_error, try_consume_token, try_token_matches};
use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, SpecifierType, Token};
use crate::parse::ast::{GlobalStatement, ValueType, VarInitialization};
use crate::parse::contextless_expression::{coalesce_type, contextualize_rvalue, detangle_initialization, detangle_typed_expr, ContextlessExpression};
use crate::parse::expression::{parse_expression, parse_initialization, parse_list};
use crate::parse::parser::{parse_body, ParserData, TokenIter, VisibilityMode};
use crate::parse::verify::context::FunctionPrototype;

pub(crate) fn parse_global_stmt(data: &mut ParserData) -> Option<GlobalStatement> {
    match data.toks.peek()? {
        Token::Specifier(_) => parse_specifier(data),

        Token::Keyword(KeywordType::Struct) => parse_struct_definition(data),
        Token::Keyword(KeywordType::Enum) => parse_enum_definition(data),
        Token::Keyword(KeywordType::Union) => parse_union_definition(data),
        Token::Keyword(KeywordType::Import) => parse_import(data),

        _ => parse_global_expression(data)
    }
}

pub(crate) fn parse_import(data: &mut ParserData) -> Option<GlobalStatement> {
    assert_token_matches!(data, Token::Keyword(KeywordType::Import));

    let mut str = String::new();

    if let Some(Token::Identifier(name)) = data.toks.peek() {
        if name.as_str() == "std" {
            str.push_str("lib/std/");
            data.toks.next();
            assert_token_matches!(data, Token::Operator(OperatorType::ScopeRes));
        }
    }

    loop {
        let Some(Token::Identifier(name)) = data.toks.next().cloned() else {
            log_error!("Expected identifier for import path");
        };
        str.push_str(name.as_str());

        if try_consume_token!(data, Token::Punctuator(PunctuatorType::Semicolon)) {
            break;
        }

        try_token_matches!(data, Token::Operator(OperatorType::Divide));
        str.push('/');
    }

    Some(
        GlobalStatement::Import { path: str }
    )
}

pub(crate) fn parse_specifier(data: &mut ParserData) -> Option<GlobalStatement> {
    assert_token_matches!(data, Token::Specifier(specifier));

    match specifier {
        SpecifierType::Public => {
            data.visibility = VisibilityMode::Public;
            try_consume_token!(data, Token::Punctuator(PunctuatorType::Colon));
            parse_global_stmt(data)
        },
        SpecifierType::Private => {
            data.visibility = VisibilityMode::Private;
            try_consume_token!(data, Token::Punctuator(PunctuatorType::Colon));
            parse_global_stmt(data)
        },

        _ => unimplemented!("parse_specifier: {:#?}", specifier)
    }
}

pub(crate) fn parse_struct_definition(data: &mut ParserData) -> Option<GlobalStatement> {
    assert_token_matches!(data, Token::Keyword(KeywordType::Struct));
    assert_token_matches!(data, Token::Identifier(name));
    let name = name.clone();
    assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenBrace));

    let mut fields = parse_list(
        data,
        Token::Punctuator(PunctuatorType::Semicolon),
        Token::Punctuator(PunctuatorType::CloseBrace),
        parse_initialization
    )?;

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseBrace));

    Some(
        GlobalStatement::TypeDeclaration {
            name: Some(name.clone()),
            type_: ValueType::Structured { fields }
        }
    )
}

pub(crate) fn parse_enum_definition(data: &mut ParserData) -> Option<GlobalStatement> {
    assert_token_matches!(data, Token::Keyword(KeywordType::Enum));
    let Some(Token::Identifier(name)) = data.toks.next().cloned() else {
        log_error!("Expected identifier for enum name");
    };
    assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenBrace));

    let mut counter = 0i32;
    let mut fields = Vec::new();

    loop {
        assert_token_matches!(data, Token::Identifier(name));
        let name = name.clone();

        if try_consume_token!(data, Token::Assignment(None)) {
            assert_token_matches!(data, Token::IntLiteral(value));
            let value = value.clone();

            fields.push((name, value as i32));
            counter = value as i32 + 1;
        } else {
            fields.push((name, counter));
            counter += 1;
        }

        if !try_consume_token!(data, Token::Punctuator(PunctuatorType::Comma)) {
            break;
        }
    }
    assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseBrace));
    try_consume_token!(data, Token::Punctuator(PunctuatorType::Semicolon));

    Some(
        GlobalStatement::Enum { name, fields }
    )
}

pub(crate) fn parse_union_definition(data: &mut ParserData) -> Option<GlobalStatement> {
    assert_token_matches!(data, Token::Keyword(KeywordType::Union));

    unimplemented!("parse_union_definition")
}

pub(crate) fn parse_global_expression(data: &mut ParserData) -> Option<GlobalStatement> {
    let expr = parse_expression(data)?;
    let Some((type_, expr)) = detangle_typed_expr(expr) else {
        log_error!("Global-scope expression must be declarative");
    };

    match expr {
        ContextlessExpression::FunctionCall {
            reference, args
        } => {
            let (return_type, header) = coalesce_type(type_, *reference)?;

            let body = Some(parse_body(data)).filter(
                |body| !body.is_empty()
            );

            let ContextlessExpression::Identifier(name) = header else {
                log_error!("Expected identifier for function name");
            };

            Some(
                GlobalStatement::Function {
                    prototype: FunctionPrototype {
                        name, return_type,
                        args: args.into_iter()
                            .map(|expr| detangle_initialization(expr.clone()))
                            .collect::<Option<Vec<_>>>()?
                    },
                    body
                }
            )
        },

        ContextlessExpression::BinaryOperation {
            op: OperatorType::ScopeRes,
            left, right
        } => {
            let ContextlessExpression::Identifier(struct_name) = *left else {
                log_error!("Expected identifier for struct name");
            };
            let ContextlessExpression::FunctionCall {
                reference, args
            } = *right else {
                log_error!("Expected function call for member function");
            };

            let (return_type, header) = coalesce_type(type_, *reference)?;

            let ContextlessExpression::Identifier(fn_name) = header else {
                log_error!("Expected identifier for member function name");
            };

            let mut args = args.into_iter()
                .skip(1)
                .map(|expr| detangle_initialization(expr.clone()))
                .collect::<Option<Vec<_>>>()?;

            args.insert(0, VarInitialization {
                name: "this".to_string(),
                type_: ValueType::PointerTo(Box::new(ValueType::Identifier(struct_name.clone())))
            });

            let body = Some(parse_body(data)).filter(
                |body| !body.is_empty()
            );

            Some(
                GlobalStatement::MemberFunction {
                    struct_parent: struct_name,
                    prototype: FunctionPrototype {
                        name: fn_name,
                        return_type,
                        args
                    },
                    body
                }
            )
        },

        tok => unimplemented!("parse_global_expression: {:#?}", tok)
    }
}