use std::clone;
use log::warn;
use crate::{assert_token_matches, log_error, try_next, try_token_matches};
use crate::lex::token::{KeywordType, PunctuatorType, Token};
use crate::parse::parser::ParserData;
use crate::parse::pass_unverified::expression::parse_identifier;
use crate::parse::pass_unverified::UVGlobalStmt;
use crate::parse::value_type::ValueType;

pub(crate) struct TypeRecord {
    pub(crate) name: Option<String>,
    pub(crate) type_: ValueType,
}

pub(crate) fn parse_typedef(data: &mut ParserData) -> Option<UVGlobalStmt> {
    let type_ = parse_type(data)?;
    let name = parse_identifier(data)?;

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

    Some(
        UVGlobalStmt::TypeDeclaration {
            name,
            type_: type_.type_
        }
    )
}

pub(crate) fn parse_plain_typedef(data: &mut ParserData) -> Option<UVGlobalStmt> {
    let Some(type_record) = parse_type(data) else {
        log_error!("PARSER ERROR: Invalid type declaration in plain typedef!");
    };
    try_next!(data, Token::Punctuator(PunctuatorType::Semicolon));

    let Some(name) = type_record.name else {
        warn!("PARSER WARNING: Plain type declaration with no name is effective no-op!");

        return Some(UVGlobalStmt::HandledInternally);
    };

    Some(
        UVGlobalStmt::TypeDeclaration {
            name,
            type_: type_record.type_
        }
    )
}

pub(crate) fn parse_type(data: &mut ParserData) -> Option<TypeRecord> {
    match data.toks.peek().expect("parse_type() called with no tokens!") {
        Token::Keyword(KeywordType::Struct) => parse_struct(data),
        Token::Keyword(KeywordType::Union) => todo!("parse_union()"),
        Token::Keyword(KeywordType::Enum) => todo!("parse_enum()"),

        Token::Identifier(_) |
        Token::Intrinsic(_) => Some(
            TypeRecord {
                name: None,
                type_: ValueType::Identifier(parse_identifier(data)?),
            }
        ),

        _ => {
            log_error!("PARSER ERROR: unknown type declaration starting with: {:#?}", data.toks.peek());
        }
    }
}

pub(crate) fn parse_struct(data: &mut ParserData) -> Option<TypeRecord> {
    assert_token_matches!(data, Token::Keyword(KeywordType::Struct));

    let name = parse_identifier(data);
    let mut fields = Vec::new();

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenBrace));

    while data.toks.peek() != Some(&Token::Punctuator(PunctuatorType::CloseBrace)) {
        let type_start_token = data.toks.peek().cloned();
        let Some(type_) = parse_type(data) else {
            log_error!("PARSER ERROR: Invalid type declaration in struct definition starting with: {:#?}", type_start_token);
        };

        let name_start_token = data.toks.peek().cloned();
        let Some(name) = parse_identifier(data) else {
            log_error!("PARSER ERROR: Invalid identifier in struct definition starting with: {:#?}", name_start_token);
        };

        fields.push((name.clone(), type_.type_));

        assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));
    }

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseBrace));

    Some(
        TypeRecord {
            name,
            type_: ValueType::Structured { fields }
        }
    )
}