use cx_data_ast::{assert_token_matches, try_next};
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, TokenKind};
use cx_data_ast::parse::ast::{CXGlobalStmt, CXTemplate, CXAST};
use cx_data_ast::parse::parser::ParserData;
use cx_util::{point_log_error, CXResult};
use crate::parse::global_scope::{parse_global_expr, parse_global_stmt};
use crate::parse::typing::parse_initializer;

pub(crate) fn parse_templated_type(data: &mut ParserData, ast: &mut CXAST) -> CXResult<CXGlobalStmt> {

}

pub(crate) fn parse_templated_generator(data: &mut ParserData, ast: &mut CXAST) -> CXResult<CXGlobalStmt> {

}

pub(crate) fn parse_templated_expr(data: &mut ParserData, ast: &mut CXAST) -> CXResult<CXTemplate> {
    assert_token_matches!(data, TokenKind::Keyword(KeywordType::Template));
    assert_token_matches!(data, TokenKind::Operator(OperatorType::Less));

    // temp_typedefs = generic_params minus already existing symbols
    let mut generic_params = Vec::new();
    let mut temp_typedefs = Vec::new();

    loop {
        assert_token_matches!(data, TokenKind::Identifier(template_name));
        let template_name = template_name.clone();
        assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::Semicolon));

        let initializer = parse_initializer(data)
            .expect("PARSER ERROR: Failed to parse initializer in template declaration!");

        let (None, return_type) = initializer else {
            point_log_error!(data, "PARSER ERROR: Template type must be anonymous! Found: {:#?}", initializer.1);
        };

        if !data.type_symbols.insert(template_name.clone()) {
            temp_typedefs.push(template_name);
        }

        if !try_next!(data, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    assert_token_matches!(data, TokenKind::Operator(OperatorType::Greater));

    let global_expr = parse_global_stmt(data)?;

    for template_name in temp_typedefs {
        data.type_symbols.remove(template_name.as_str());
    }

    Some(
        CXTemplate {
            generic_types: generic_params,
            body: global_expr,
        }
    )
}