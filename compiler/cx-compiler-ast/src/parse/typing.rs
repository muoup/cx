use cx_data_ast::{assert_token_matches, try_next};
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::parse::expression::parse_name;
use cx_data_ast::parse::ast::{TypeMap, CXAST};
use cx_data_ast::parse::identifier::{parse_intrinsic, parse_std_ident, CXIdent};
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::parse::value_type::CXValType;
use cx_util::log_error;
use crate::parse::parsing_tools::{goto_block_end, goto_statement_end};

pub(crate) struct TypeRecord {
    pub(crate) name: Option<String>,
    pub(crate) type_: CXValType,
}

pub fn is_type_decl(data: &mut ParserData) -> bool {
    let tok = data.toks.peek();

    if tok.is_none() {
        return false;
    }

    match tok.unwrap() {
        Token::Intrinsic(_) |
        Token::Keyword(KeywordType::Struct) |
        Token::Keyword(KeywordType::Union) |
        Token::Keyword(KeywordType::Enum) => true,

        Token::Identifier(name) => data.type_symbols.contains(name),

        _ => false
    }
}

pub fn parse_types(data: &mut ParserData) -> Option<TypeMap> {
    let mut type_map = TypeMap::new();

    while let Some(token) = data.toks.next() {
        let type_record = match token {
            Token::Keyword(KeywordType::Typedef) =>
                parse_typedef(data)?,
            Token::Keyword(KeywordType::Struct) |
            Token::Keyword(KeywordType::Enum) |
            Token::Keyword(KeywordType::Union) =>
                parse_plain_typedef(data)?,

            _ => {
                goto_statement_end(data);
                continue;
            }
        };

        if let Some(name) = type_record.name {
            type_map.insert(name.clone(), type_record.type_);
            data.type_symbols.insert(name);
        }
    }

    Some(type_map)
}

pub(crate) fn parse_typedef(data: &mut ParserData) -> Option<TypeRecord> {
    let type_ = parse_type(data)?;
    let name = parse_name(data)?;

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

    Some(
        TypeRecord {
            name: Some(name),
            type_: type_.type_
        }
    )
}

pub(crate) fn parse_plain_typedef(data: &mut ParserData) -> Option<TypeRecord> {
    let Some(type_record) = parse_type(data) else {
        log_error!("PARSER ERROR: Invalid type declaration in plain typedef!");
    };
    try_next!(data, Token::Punctuator(PunctuatorType::Semicolon));

    Some(type_record)
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

fn parse_typemod_name(data: &mut ParserData, acc_type: CXValType) -> Option<(Option<CXIdent>, CXValType)> {
    let Some(next_tok) = data.toks.peek() else {
        return Some((None, acc_type));
    };

    match next_tok {
        Token::Operator(OperatorType::Asterisk) => {
            data.toks.next();
            parse_typemod_name(data, CXValType::PointerTo(Box::new(acc_type)))
        },

        Token::Identifier(_) => Some((Some(parse_std_ident(data)?), acc_type)),

        _ => Some((None, acc_type))
    }
}

fn parse_type_base(data: &mut ParserData) -> Option<CXValType> {
    match data.toks.peek()? {
        Token::Identifier(_) => Some(CXValType::Identifier(parse_std_ident(data)?)),
        Token::Intrinsic(_) => Some(CXValType::Identifier(parse_intrinsic(data)?)),

        Token::Keyword(KeywordType::Struct) => {
            let Some(type_record) = parse_struct(data) else {
                log_error!("PARSER ERROR: Invalid type declaration in struct definition!");
            };

            Some(type_record.type_)
        },

        _ => log_error!("Unknown base to type initializer: {:#?}", data.toks.peek()),
    }
}

pub(crate) fn parse_initializer(data: &mut ParserData) -> Option<(Option<CXIdent>, CXValType)> {
    let type_base = parse_type_base(data)?;

    parse_typemod_name(data, type_base)
}