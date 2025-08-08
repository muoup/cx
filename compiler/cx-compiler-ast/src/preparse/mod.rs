pub mod typing;
pub mod preparser;

mod macros;
mod importing;

use crate::preparse::preparser::{preparse_stmt, PreparseResult};
use cx_data_ast::parse::parser::VisibilityMode;
use cx_data_ast::PreparseContents;
use cx_data_lexer::TokenIter;
use cx_util::{log_error, point_log_error};

pub(crate) struct PreparseData<'a> {
    tokens: TokenIter<'a>,
    visibility_mode: VisibilityMode,
}

pub fn preparse(tokens: TokenIter) -> Option<PreparseContents> {
    let mut data = PreparseData {
        tokens,
        visibility_mode: VisibilityMode::Private,
    };
    
    let mut contents = PreparseContents::default();

    while data.tokens.has_next() {
        let Some(result) = preparse_stmt(&mut data) else {
            point_log_error!(data.tokens, "Failed to preparse statement")
        };

        match result {
            PreparseResult::TypeDefinition(name, mut type_) => {
                type_.set_visibility_mode(data.visibility_mode);
                contents.type_definitions.insert(name, type_);
            },

            PreparseResult::FunctionDefinition(signature) => {
                contents.function_definitions.push((signature.name.to_string(), signature));
            },

            PreparseResult::DestructorDefinition(name) => {
                contents.destructor_definitions.push(name);
            },

            PreparseResult::Import(path) => {
                contents.imports.push(path);
            },

            PreparseResult::TypeTemplate(template) => {
                contents.type_templates.push(template);
            },

            PreparseResult::FunctionTemplate(template) => {
                contents.function_templates.push(template);
            },

            PreparseResult::Nothing => {}
        } 
    }

    Some(contents)
}

