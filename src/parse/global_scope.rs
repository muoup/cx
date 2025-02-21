use crate::{assert_token_matches, log_error};
use crate::lex::token::{KeywordType, PunctuatorType, Token};
use crate::parse::ast::{GlobalStatement, UnverifiedGlobalStatement, ValueType, VarInitialization};
use crate::parse::contextless_expression::{coalesce_type, detangle_initialization, detangle_typed_expr, ContextlessExpression};
use crate::parse::expression::{parse_expression, parse_initialization, parse_list};
use crate::parse::parser::{parse_body, TokenIter};
use crate::parse::verify::context::FunctionPrototype;

pub(crate) fn parse_struct_definition(toks: &mut TokenIter) -> Option<GlobalStatement> {
    assert_token_matches!(toks, Token::Keyword(KeywordType::Struct));
    assert_token_matches!(toks, Token::Identifier(name));
    let name = name.clone();
    assert_token_matches!(toks, Token::Punctuator(PunctuatorType::OpenBrace));

    let mut fields = parse_list(
        toks,
        Token::Punctuator(PunctuatorType::Semicolon),
        Token::Punctuator(PunctuatorType::CloseBrace),
        parse_initialization
    )?;

    assert_token_matches!(toks, Token::Punctuator(PunctuatorType::CloseBrace));

    Some(
        GlobalStatement::TypeDeclaration {
            name: Some(name.clone()),
            type_: ValueType::Structured { fields }
        }
    )
}

pub(crate) fn parse_enum_definition(toks: &mut TokenIter) -> Option<GlobalStatement> {
    assert_token_matches!(toks, Token::Keyword(KeywordType::Enum));

    unimplemented!("parse_enum_definition")
}

pub(crate) fn parse_union_definition(toks: &mut TokenIter) -> Option<GlobalStatement> {
    assert_token_matches!(toks, Token::Keyword(KeywordType::Union));

    unimplemented!("parse_union_definition")
}

pub(crate) fn parse_global_expression(toks: &mut TokenIter) -> Option<GlobalStatement> {
    let expr = parse_expression(toks)?;
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

            if let Some(&Token::Punctuator(PunctuatorType::Semicolon)) = toks.peek() {
                toks.next();
                return Some(
                    GlobalStatement::Function {
                        prototype,
                        body: None
                    }
                )
            }

            return Some(
                GlobalStatement::Function {
                    prototype,
                    body: Some(parse_body(toks))
                }
            )
        },

        tok => unimplemented!("parse_global_expression: {:#?}", tok)
    }
}

pub(crate) fn parse_global_stmt(toks: &mut TokenIter) -> Option<GlobalStatement> {
    match toks.peek()? {
        Token::Keyword(KeywordType::Struct) => parse_struct_definition(toks),
        Token::Keyword(KeywordType::Enum) => parse_enum_definition(toks),
        Token::Keyword(KeywordType::Union) => parse_union_definition(toks),

        _ => parse_global_expression(toks)
    }
}