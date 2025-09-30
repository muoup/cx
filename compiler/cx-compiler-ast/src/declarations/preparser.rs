use cx_data_ast::{assert_token_matches, peek_next, try_next};
use crate::preparse::typing::{parse_enum, parse_initializer, parse_params, parse_struct, parse_template_args, parse_union};
use cx_data_lexer::token::{OperatorType, PunctuatorType, TokenKind};
use cx_util::identifier::CXIdent;
use cx_data_ast::parse::parser::{VisibilityMode};
use cx_data_ast::preparse::CXNaiveFnIdent;
use cx_data_ast::preparse::naive_types::{CXNaivePrototype, CXNaiveType, CXNaiveTypeKind, PredeclarationType};
use cx_data_ast::preparse::templates::{CXFunctionTemplate, CXTemplatePrototype, CXTypeTemplate};
use cx_data_lexer::{keyword, operator, punctuator, specifier, TokenIter};
use cx_util::{log_error, CXResult};
use crate::parse::global_scope::destructor_prototype;
use crate::preparse::importing::parse_import;
use crate::preparse::PreparseData;

pub(crate) enum PreparseResult {
    TypeDefinition(String, CXNaiveType),
    FunctionDefinition(CXNaivePrototype),
    DestructorDefinition(CXNaiveType),
    TypeTemplate(CXTypeTemplate),
    FunctionTemplate(CXFunctionTemplate),

    Import(String),

    Nothing
}

pub(crate) fn parse_template(data: &mut PreparseData) -> Option<PreparseResult> {
    assert_token_matches!(data.tokens, keyword!(Template));
    assert_token_matches!(data.tokens, operator!(Less));
    
    let mut type_decls = Vec::new();
    
    loop {
        assert_token_matches!(data.tokens, TokenKind::Identifier(template_name));
        let template_name = template_name.clone();
        
        assert_token_matches!(data.tokens, punctuator!(Colon));
        assert_token_matches!(data.tokens, keyword!(Type));
    
        type_decls.push(template_name);
        
        if !try_next!(data.tokens, operator!(Comma)) {
            break;
        }
    }
    
    assert_token_matches!(data.tokens, operator!(Greater));
    
    let stmt = preparse_stmt(data)
        .expect("Failed to parse global expression in template declaration!");
    
    match stmt {
        PreparseResult::FunctionDefinition(signature) => {
            Some(
                PreparseResult::FunctionTemplate(
                    CXFunctionTemplate {
                        name: CXIdent::from(signature.name.mangle()),
                        prototype: CXTemplatePrototype {
                            types: type_decls.clone(),
                        },
                        shell: signature
                    }
                )
            )
        },

        PreparseResult::TypeDefinition(name, type_) => {
            Some(
                PreparseResult::TypeTemplate(
                    CXTypeTemplate {
                        name: CXIdent::from(name),
                        prototype: CXTemplatePrototype {
                            types: type_decls.clone(),
                        },
                        shell: type_
                    }
                )
            )
        },

        PreparseResult::DestructorDefinition(_type) => {
            let prototype = destructor_prototype(_type.clone());

            Some(
                PreparseResult::FunctionTemplate(
                    CXFunctionTemplate {
                        name: CXIdent::from(prototype.name.mangle()),
                        prototype: CXTemplatePrototype {
                            types: type_decls.clone(),
                        },
                        shell: prototype
                    }
                )
            )
        },

        _ => log_preparse_error!(data.tokens, "Invalid statement in template declaration!"),
    }
}

