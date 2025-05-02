use log::warn;
use crate::{assert_token_matches, log_error, try_next};
use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::parse::parser::ParserData;
use crate::parse::pass_unverified::expression::{parse_identifier, parse_name};
use crate::parse::pass_unverified::{UVGlobalStmt, UVIdent};
use crate::parse::value_type::CXValType;

pub(crate) struct TypeRecord {
    pub(crate) name: Option<String>,
    pub(crate) type_: CXValType,
}

pub(crate) fn parse_typedef(data: &mut ParserData) -> Option<UVGlobalStmt> {
    let type_ = parse_type(data)?;
    let name = parse_name(data)?;

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
                type_: parse_type_base(data)?
            }
        ),

        _ => {
            log_error!("PARSER ERROR: unknown type declaration starting with: {:#?}", data.toks.peek());
        }
    }
}

pub(crate) fn parse_struct(data: &mut ParserData) -> Option<TypeRecord> {
    assert_token_matches!(data, Token::Keyword(KeywordType::Struct));

    let name = parse_name(data);
    let mut fields = Vec::new();

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::OpenBrace));

    while data.toks.peek() != Some(&Token::Punctuator(PunctuatorType::CloseBrace)) {
        let type_start_token = data.toks.peek().cloned();
        let Some(type_) = parse_type(data) else {
            log_error!("PARSER ERROR: Invalid type declaration in struct definition starting with: {:#?}", type_start_token);
        };

        let name_start_token = data.toks.peek().cloned();
        let Some(name) = parse_name(data) else {
            log_error!("PARSER ERROR: Invalid identifier in struct definition starting with: {:#?}", name_start_token);
        };

        fields.push((name, type_.type_));

        assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));
    }

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseBrace));

    Some(
        TypeRecord {
            name,
            type_: CXValType::Structured { fields }
        }
    )
}

fn parse_typemod_name(data: &mut ParserData, acc_type: CXValType) -> Option<(Option<UVIdent>, CXValType)> {
    let Some(next_tok) = data.toks.peek() else {
        log_error!("PARSER ERROR: Expected type modifier token, found EOF!");
    };

    match next_tok {
        Token::Operator(OperatorType::Asterisk) => {
            data.toks.next();
            parse_typemod_name(data, CXValType::PointerTo(Box::new(acc_type)))
        },

        Token::Identifier(_) => Some((Some(parse_identifier(data)?), acc_type)),

        _ => Some((None, acc_type))
    }
}

fn parse_type_base(data: &mut ParserData) -> Option<CXValType> {
    match data.toks.next()? {
        Token::Identifier(name) =>
            Some(CXValType::Identifier(name.clone())),
        Token::Intrinsic(_) => {
            data.toks.back();
            Some(CXValType::Identifier(parse_intrinisic(data)?))
        }

        _ => log_error!("Unknown base to type initializer: {:#?}", data.toks.peek()),
    }
}

pub(crate) fn parse_intrinisic(data: &mut ParserData) -> Option<String> {
    let mut name = String::new();

    while let Some(Token::Intrinsic(intrinsic)) = data.toks.next() {
        name.push_str(format!("{:?}", intrinsic).to_lowercase().as_str());
    }

    data.toks.back();

    Some(name)
}

pub(crate) fn parse_initializer(data: &mut ParserData) -> Option<(Option<UVIdent>, CXValType)> {
    let type_base = parse_type_base(data)?;

    parse_typemod_name(data, type_base)
}