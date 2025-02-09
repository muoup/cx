use std::assert_matches::assert_matches;
use log::warn;
use crate::assert_token_matches;
use crate::lex::token::{KeywordType, Token};
use crate::parse::ast::GlobalStatement;
use crate::parse::parser::TokenIter;
use crate::parse::verify::ValueTypeRef;

pub(crate) fn parse_global_stmt(toks: &mut TokenIter) -> Option<GlobalStatement> {
    match toks.peek()? {
        Token::Keyword(KeywordType::Typedef) => {
            toks.next();
            let type_= parse_type_definition(toks)?;
            let Token::Identifier(name) = toks.next()? else {
                warn!("Expected identifier name after typedef");
                return None;
            };

            Some(GlobalStatement::TypeDeclaration {
                name: Some(name.clone()),
                type_
            })
        },
        _ => {
            let type_ = parse_type_definition(toks)?;
            let Token::Identifier(name) = toks.next()? else {
                warn!("Expected identifier name after type declaration");
                return None;
            };
        }
    }
}

pub(crate) fn parse_type_definition(toks: &mut TokenIter) -> Option<ValueTypeRef> {
    match toks.peek()? {
        Token::Keyword(KeywordType::Struct) => parse_struct_definition(toks),
        Token::Keyword(KeywordType::Enum) => parse_enum_definition(toks),
        Token::Keyword(KeywordType::Union) => parse_union_definition(toks),

        _ => {
            unimplemented!("parse_type_definition")
        }
    }
}

pub(crate) fn parse_struct_definition(toks: &mut TokenIter) -> Option<ValueTypeRef> {
    assert_token_matches!(Token::Keyword(KeywordType::Struct));

    unimplemented!("parse_struct_definition")
}

pub(crate) fn parse_enum_definition(toks: &mut TokenIter) -> Option<ValueTypeRef> {
    assert_token_matches!(Token::Keyword(KeywordType::Enum));

    unimplemented!("parse_enum_definition")
}

pub(crate) fn parse_union_definition(toks: &mut TokenIter) -> Option<ValueTypeRef> {
    assert_token_matches!(Token::Keyword(KeywordType::Union));

    unimplemented!("parse_union_definition")
}