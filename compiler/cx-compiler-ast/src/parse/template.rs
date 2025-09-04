use cx_data_ast::{assert_token_matches, try_next};
use cx_data_lexer::token::{KeywordType, OperatorType, PunctuatorType, TokenKind};
use cx_data_ast::parse::ast::CXGlobalStmt;
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::parser::ParserData;
use cx_data_ast::preparse::naive_types::{CXNaiveType, CXNaiveTypeKind, ModuleResource, PredeclarationType};
use cx_util::{point_log_error, CXResult};
use crate::parse::global_scope::{destructor_prototype, parse_global_stmt};

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
        
        if !data.ast.type_map.standard.contains_key(template_name.as_str()) {
            let _nil_type: CXNaiveType = CXNaiveTypeKind::Identifier {
                name: CXIdent::from("__undefined_template_type"),
                predeclaration: PredeclarationType::None
            }.to_type();

            data.ast.type_map
                .insert_standard(template_name.clone(), ModuleResource::with_visibility(_nil_type, data.visibility));
            temp_typedefs.push(template_name);
        }

        if !try_next!(data.tokens, TokenKind::Operator(OperatorType::Comma)) {
            break;
        }
    }

    assert_token_matches!(data.tokens, TokenKind::Operator(OperatorType::Greater));

    let global_stmt = parse_global_stmt(data)?;

    for template_name in temp_typedefs {
        data.ast.type_map.standard.remove(template_name.as_str());
    }

    let Some(global_expr) = global_stmt else {
        return Some(None);
    };

    match global_expr {
        CXGlobalStmt::FunctionDefinition { prototype, body } => {
            Some(
                Some(
                    CXGlobalStmt::TemplatedFunction { prototype, body }
                ) 
            )
        },

        CXGlobalStmt::DestructorDefinition { _type, body } => {
            Some(
                Some(
                    CXGlobalStmt::TemplatedFunction {
                        prototype: destructor_prototype(_type.clone()),
                        body,
                    }
                )
            )
        },

        CXGlobalStmt::TypeDecl { .. } => todo!(),

        CXGlobalStmt::TemplatedFunction { .. } =>
            point_log_error!(data.tokens, "PARSER ERROR: Nested templated generators are not supported!"),
        CXGlobalStmt::GlobalVariable { .. } =>
            point_log_error!(data.tokens, "PARSER ERROR: Templated global variables are not supported!"),
        CXGlobalStmt::FunctionPrototype { .. } =>
            point_log_error!(data.tokens, "PARSER ERROR: Templated functions predeclarations are not supported!"),
    }
}