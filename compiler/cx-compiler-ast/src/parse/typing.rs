use cx_data_ast::{assert_token_matches, try_next};
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::parse::expression::parse_name;
use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::identifier::{parse_intrinsic, parse_std_ident, CXIdent};
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::parse::value_type::CXValType;
use cx_util::log_error;

pub(crate) struct TypeRecord {
    pub(crate) name: Option<String>,
    pub(crate) type_: CXValType,
}

pub(crate) fn parse_typedef(data: &mut ParserData, ast: &mut CXAST) -> Option<()> {
    let type_ = parse_type(data)?;
    let name = parse_name(data)?;

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

    ast.type_map.insert(name, type_.type_);
    Some(())
}

pub(crate) fn parse_plain_typedef(data: &mut ParserData, ast: &mut CXAST) -> Option<()> {
    let Some(type_record) = parse_type(data) else {
        log_error!("PARSER ERROR: Invalid type declaration in plain typedef!");
    };
    try_next!(data, Token::Punctuator(PunctuatorType::Semicolon));

    let Some(name) = type_record.name else {
        println!("PARSER WARNING: Plain type declaration with no name is effective no-op!");

        return Some(())
    };

    ast.type_map.insert(
        name.clone(),
        type_record.type_.clone()
    );

    Some(())
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
        log_error!("PARSER ERROR: Expected type modifier token, found EOF!");
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

        _ => log_error!("Unknown base to type initializer: {:#?}", data.toks.peek()),
    }
}

pub(crate) fn parse_initializer(data: &mut ParserData) -> Option<(Option<CXIdent>, CXValType)> {
    let type_base = parse_type_base(data)?;

    parse_typemod_name(data, type_base)
}