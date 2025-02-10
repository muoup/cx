use std::clone;
use cranelift::codegen::ir::Expr;
use log::warn;
use crate::{assert_token_matches, try_token_matches};
use crate::lex::token::{KeywordType, PunctuatorType, Token};
use crate::parse::ast::{Expression, GlobalStatement, UnverifiedExpression};
use crate::parse::expression::parse_expression;
use crate::parse::parser::{parse_body, TokenIter};
use crate::parse::type_expr::parse_identifier;
use crate::parse::verify::ValueTypeRef;

#[derive(Debug, Clone)]
pub enum ExprType {
    Verified(ValueTypeRef),

    AnonymousStruct(Vec<(String, ExprType)>),
    AnonymousEnum(Vec<(String, i64)>),
    AnonymousUnion(Vec<(String, ExprType)>),

    Unverified(String)
}

pub(crate) fn parse_global_stmt(toks: &mut TokenIter) -> Option<GlobalStatement> {
    if let Token::Keyword(KeywordType::Typedef) = toks.peek()? {
        toks.next();
        let type_= parse_type_definition(toks)?;
        assert_token_matches!(toks, Token::Identifier(name));

        return Some(
            GlobalStatement::TypeDeclaration {
                name: Some(name.clone()),
                type_
            }
        )
    }

    let expr = parse_expression(toks)?;
    let Expression::Unverified(UnverifiedExpression::CompoundIdentifier { ident, suffix })
        = expr else {
        println!("Found global statement {:?}", expr);
        return None;
    };

    match suffix.as_ref() {
        Expression::Unverified(UnverifiedExpression::FunctionCall { name, args }) => {
            println!("Function call: {:?}({:?})", name, args);

            let Expression::Unverified(UnverifiedExpression::Identifier(method_name)) = name.as_ref() else {
                warn!("Expected identifier, found {:?}", name);
                return None;
            };

            let body = match toks.peek()? {
                Token::Punctuator(PunctuatorType::Semicolon) => {
                    None
                },

                _ => Some(parse_body(toks))
            };

            Some(
                GlobalStatement::Function {
                    name: ident,
                    arguments: args.clone(),
                    return_type: ExprType::Unverified(method_name.clone()),
                    body
                }
            )
        },

        _ => unimplemented!()
    }
}

pub(crate) fn parse_type_definition(toks: &mut TokenIter) -> Option<ExprType> {
    match toks.peek()? {
        Token::Keyword(KeywordType::Struct) => parse_struct_definition(toks),
        Token::Keyword(KeywordType::Enum) => parse_enum_definition(toks),
        Token::Keyword(KeywordType::Union) => parse_union_definition(toks),
        Token::Identifier(_) | Token::Intrinsic(_) => {
            let type_ = parse_identifier(toks)?;
            Some(ExprType::Unverified(type_.to_string()))
        },

        tok => {
            unimplemented!("parse_type_definition_default: {:?}", tok)
        }
    }
}

pub(crate) fn parse_struct_definition(toks: &mut TokenIter) -> Option<ExprType> {
    assert_token_matches!(toks, Token::Keyword(KeywordType::Struct));
    assert_token_matches!(toks, Token::Identifier(name));
    assert_token_matches!(toks, Token::Punctuator(PunctuatorType::OpenBrace));

    let mut fields = Vec::new();

    while let Some(type_) = parse_type_definition(toks) {
        assert_token_matches!(toks, Token::Identifier(name));
        fields.push((name.clone(), type_));
        assert_token_matches!(toks, Token::Punctuator(PunctuatorType::Semicolon));
    }

    assert_token_matches!(toks, Token::Punctuator(PunctuatorType::CloseBrace));

    Some(ExprType::AnonymousStruct(fields))
}

pub(crate) fn parse_enum_definition(toks: &mut TokenIter) -> Option<ExprType> {
    assert_token_matches!(toks, Token::Keyword(KeywordType::Enum));

    unimplemented!("parse_enum_definition")
}

pub(crate) fn parse_union_definition(toks: &mut TokenIter) -> Option<ExprType> {
    assert_token_matches!(toks, Token::Keyword(KeywordType::Union));

    unimplemented!("parse_union_definition")
}