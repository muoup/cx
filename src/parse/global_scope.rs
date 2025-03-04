use crate::{assert_token_matches, log_error, try_consume_token, try_token_matches};
use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::parse::ast::{GlobalStatement, UnverifiedGlobalStatement, ValueType, VarInitialization};
use crate::parse::contextless_expression::{coalesce_type, detangle_initialization, detangle_typed_expr, ContextlessExpression};
use crate::parse::expression::{parse_expression, parse_initialization, parse_list};
use crate::parse::parser::{parse_body, ParserData, TokenIter};
use crate::parse::verify::context::FunctionPrototype;

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
            let (type_, body) = coalesce_type(type_, *reference)?;

            let ContextlessExpression::Identifier(name) = body else {
                log_error!("Expected identifier for function call, found: {:#?}", body);
            };

            let prototype = FunctionPrototype {
                return_type: type_,
                name,
                args: args.iter()
                    .map(|expr| detangle_initialization(expr.clone()))
                    .collect::<Option<Vec<_>>>()?
            };

            if let Some(&Token::Punctuator(PunctuatorType::Semicolon)) = data.toks.peek() {
                data.toks.next();
                return Some(
                    GlobalStatement::Function {
                        prototype,
                        body: None
                    }
                )
            }

            Some(
                GlobalStatement::Function {
                    prototype,
                    body: Some(parse_body(data))
                }
            )
        },

        tok => unimplemented!("parse_global_expression: {:#?}", tok)
    }
}

pub(crate) fn parse_global_stmt(data: &mut ParserData) -> Option<GlobalStatement> {
    match data.toks.peek()? {
        Token::Keyword(KeywordType::Struct) => parse_struct_definition(data),
        Token::Keyword(KeywordType::Enum) => parse_enum_definition(data),
        Token::Keyword(KeywordType::Union) => parse_union_definition(data),

        _ => parse_global_expression(data)
    }
}