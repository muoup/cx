use crate::assert_token_matches;
use crate::parse::ast::ValueType;
use crate::parse::expression::parse_identifier;
use crate::parse::parser::ParserData;
use crate::parse::pass_unverified::UVGlobalStmt;

pub(crate) fn parse_type_decl(data: &mut ParserData) -> Option<UVGlobalStmt> {
    let type_ = parse_type(data)?;
    let name = parse_identifier(data)?;

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::Semicolon));

    Some(
        UVGlobalStmt::TypeDeclaration {
            name: Some(name),
            type_
        }
    )
}

pub(crate) fn parse_type(data: &mut ParserData) -> Option<ValueType> {

}