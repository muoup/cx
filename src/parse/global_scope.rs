use log::warn;
use crate::assert_token_matches;
use crate::lex::token::{KeywordType, PunctuatorType, Token};
use crate::parse::ast::{Expression, GlobalStatement, UnverifiedExpression, UnverifiedGlobalStatement, ValueExpression, ValueType};
use crate::parse::expression::{parse_expression, parse_expressions, parse_identifier};
use crate::parse::parser::{parse_body, TokenIter};

pub(crate) fn parse_struct_definition(toks: &mut TokenIter) -> Option<UnverifiedGlobalStatement> {
    assert_token_matches!(toks, Token::Keyword(KeywordType::Struct));
    assert_token_matches!(toks, Token::Identifier(name));
    let name = name.clone();
    assert_token_matches!(toks, Token::Punctuator(PunctuatorType::OpenBrace));

    let mut fields = parse_expressions(
        toks,
        Token::Punctuator(PunctuatorType::Semicolon),
        Token::Punctuator(PunctuatorType::CloseBrace),
    )?;

    assert_token_matches!(toks, Token::Punctuator(PunctuatorType::CloseBrace));

    Some(
        UnverifiedGlobalStatement::StructDeclaration {
            name: name.clone(),
            field_declarations: fields
        }
    )
}

pub(crate) fn parse_enum_definition(toks: &mut TokenIter) -> Option<UnverifiedGlobalStatement> {
    assert_token_matches!(toks, Token::Keyword(KeywordType::Enum));

    unimplemented!("parse_enum_definition")
}

pub(crate) fn parse_union_definition(toks: &mut TokenIter) -> Option<UnverifiedGlobalStatement> {
    assert_token_matches!(toks, Token::Keyword(KeywordType::Union));

    unimplemented!("parse_union_definition")
}

pub(crate) fn parse_global_expression(toks: &mut TokenIter) -> Option<UnverifiedGlobalStatement> {
    let expr = parse_expression(toks)?;
    let Expression::Unverified(
        UnverifiedExpression::CompoundExpression {
            prefix: type_, suffix, ..
        }
    ) = expr else {
        println!("Global Expressions must be declarative, found: {:?}", expr);
        return None;
    };

    match *suffix {
        Expression::Unverified(
            UnverifiedExpression::FunctionCall { name, args }
        ) => {
            let body = match toks.peek()? {
                Token::Punctuator(PunctuatorType::Semicolon) => {
                    toks.next();
                    None
                }

                _ => Some(parse_body(toks))
            };

            Some(
                UnverifiedGlobalStatement::Function {
                    return_type: *type_,
                    name_header: *name,
                    params: args,
                    body,
                }
            )
        },
        Expression::Value(
            ValueExpression::Assignment { left, right, operator }
        ) => {
            assert_eq!(operator, None);
            assert_token_matches!(toks, Token::Punctuator(PunctuatorType::Semicolon));

            Some(
                UnverifiedGlobalStatement::GlobalVariable {
                    left: *left,
                    value: Some(*right)
                }
            )
        },

        tok => unimplemented!("parse_global_expression: {:?}", tok)
    }
}

pub(crate) fn parse_global_stmt(toks: &mut TokenIter) -> Option<UnverifiedGlobalStatement> {
    match toks.peek()? {
        Token::Keyword(KeywordType::Struct) => parse_struct_definition(toks),
        Token::Keyword(KeywordType::Enum) => parse_enum_definition(toks),
        Token::Keyword(KeywordType::Union) => parse_union_definition(toks),

        _ => parse_global_expression(toks)
    }
}