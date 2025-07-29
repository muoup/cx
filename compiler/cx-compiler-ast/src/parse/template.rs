use cx_data_ast::{assert_token_matches, try_next};
use cx_data_ast::lex::token::{KeywordType, OperatorType, PunctuatorType, TokenKind};
use cx_data_ast::parse::ast::{CXGlobalStmt, CXAST};
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::parse::template::CXTemplateTypeGen;
use cx_util::{point_log_error, CXResult};
use crate::parse::global_scope::parse_global_stmt;

pub(crate) fn parse_template(data: &mut ParserData, ast: &mut CXAST) -> CXResult<Option<CXGlobalStmt>> {
    assert_token_matches!(data.toks, TokenKind::Keyword(KeywordType::Template));
    assert_token_matches!(data.toks, TokenKind::Operator(OperatorType::Less));

    // temp_typedefs = generic_params minus already existing symbols
    let mut generic_params = Vec::new();
    let mut temp_typedefs = Vec::new();

    loop {
        assert_token_matches!(data.toks, TokenKind::Identifier(template_name));
        let template_name = template_name.clone();
        assert_token_matches!(data.toks, TokenKind::Punctuator(PunctuatorType::Colon));
        assert_token_matches!(data.toks, TokenKind::Keyword(KeywordType::Type));

        generic_params.push(template_name.clone());
        if !data.type_symbols.insert(template_name.clone()) {
            temp_typedefs.push(template_name);
        }

        if !try_next!(data.toks, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    assert_token_matches!(data.toks, TokenKind::Operator(OperatorType::Greater));

    let global_expr = parse_global_stmt(data, ast)?
        .expect("PARSER ERROR: Failed to parse global expression in template declaration!");

    for template_name in temp_typedefs {
        data.type_symbols.remove(template_name.as_str());
    }

    match global_expr {
        CXGlobalStmt::FunctionDefinition { prototype, body } => {
            let template = CXTemplateTypeGen::function_template(generic_params.clone(), prototype.clone());
            let prototype_name = prototype.name.clone();

            ast.function_map.insert_template(prototype_name.to_string(), template.clone());

            Some(
                Some(
                    CXGlobalStmt::TemplatedFunction {
                        fn_name: prototype_name,
                        body,
                    }
                )
            )
        },

        CXGlobalStmt::TypeDecl { .. } => todo!(),
        CXGlobalStmt::DestructorDefinition { .. } => todo!(),

        CXGlobalStmt::TemplatedFunction { .. } =>
            point_log_error!(data.toks, "PARSER ERROR: Nested templated generators are not supported!"),
        CXGlobalStmt::GlobalVariable { .. } =>
            point_log_error!(data.toks, "PARSER ERROR: Templated global variables are not supported!"),
        CXGlobalStmt::FunctionPrototype { .. } =>
            point_log_error!(data.toks, "PARSER ERROR: Templated functions must be declared with a function definition!"),
    }
}