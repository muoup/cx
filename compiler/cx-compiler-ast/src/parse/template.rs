use cx_data_ast::{assert_token_matches, try_next};
use cx_data_lexer::token::{KeywordType, OperatorType, PunctuatorType, TokenKind};
use cx_data_ast::parse::ast::{CXExpr, CXGlobalStmt, CXAST};
use cx_data_ast::parse::parser::{ParserData, TokenIter};
use cx_data_ast::parse::template::{CXTemplateInput, CXTemplateTypeGen};
use cx_data_ast::parse::value_type::CXType;
use cx_util::{point_log_error, CXResult};
use crate::parse::global_scope::parse_global_stmt;
use crate::preparse::typing::parse_initializer;

pub(crate) fn parse_template(data: &mut ParserData) -> CXResult<Option<CXGlobalStmt>> {
    assert_token_matches!(data.tokens, TokenKind::Keyword(KeywordType::Template));
    assert_token_matches!(data.tokens, TokenKind::Operator(OperatorType::Less));

    // temp_typedefs = generic_params minus already existing symbols
    let mut generic_params = Vec::new();
    let mut temp_typedefs = Vec::new();

    loop {
        assert_token_matches!(data.tokens, TokenKind::Identifier(template_name));
        let template_name = template_name.clone();
        assert_token_matches!(data.tokens, TokenKind::Punctuator(PunctuatorType::Colon));
        assert_token_matches!(data.tokens, TokenKind::Keyword(KeywordType::Type));

        generic_params.push(template_name.clone());
        
        if !data.ast.type_map.contains_key(template_name.as_str()) {
            data.ast.type_map.insert(template_name.clone(), CXType::unit());
            temp_typedefs.push(template_name);
        }

        if !try_next!(data.tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    assert_token_matches!(data.tokens, TokenKind::Operator(OperatorType::Greater));

    let Some(global_expr) = parse_global_stmt(data)? else {
        return Some(None);
    };

    for template_name in temp_typedefs {
        data.ast.type_map.remove(template_name.as_str());
    }

    match global_expr {
        CXGlobalStmt::FunctionDefinition { prototype, body } => {
            let template = CXTemplateTypeGen::function_template(generic_params.clone(), prototype.clone());
            let prototype_name = prototype.name.clone();

            data.ast.function_map.insert_template(prototype_name.to_string(), template.clone());

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
            point_log_error!(data.tokens, "PARSER ERROR: Nested templated generators are not supported!"),
        CXGlobalStmt::GlobalVariable { .. } =>
            point_log_error!(data.tokens, "PARSER ERROR: Templated global variables are not supported!"),
        CXGlobalStmt::FunctionPrototype { .. } =>
            point_log_error!(data.tokens, "PARSER ERROR: Templated functions must be declared with a function definition!"),
    }
}

pub fn parse_template_args(tokens: &mut TokenIter) -> CXResult<CXTemplateInput> {
    assert_token_matches!(tokens, TokenKind::Operator(OperatorType::Less));

    let mut input_types = Vec::new();

    loop {
        let Some((None, _type)) = parse_initializer(tokens) else {
            point_log_error!(tokens, "PARSER ERROR: Expected type declaration in template arguments!");
        };
        
        input_types.push(_type);
        
        if !try_next!(tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    assert_token_matches!(tokens, TokenKind::Operator(OperatorType::Greater));

    Some(CXTemplateInput { types: input_types })
}