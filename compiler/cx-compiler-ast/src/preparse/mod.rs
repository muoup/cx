pub mod typing;
pub mod preparser;

mod macros;
mod importing;

use cx_data_ast::parse::identifier::CXIdent;
use crate::preparse::preparser::{preparse_stmt, PreparseResult};
use cx_data_ast::parse::parser::VisibilityMode;
use cx_data_ast::preparse::pp_type::{CXNaiveType, ModuleResource};
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
            PreparseResult::TypeDefinition(name, type_) => {
                contents.type_definitions.insert(name, ModuleResource::with_visibility(type_, data.visibility_mode));
            },

            PreparseResult::FunctionDefinition(signature) => {
                contents.function_definitions.push(ModuleResource::with_visibility(signature, data.visibility_mode));
            },

            PreparseResult::DestructorDefinition(name) => {
                contents.destructor_definitions.push(name);
            },

            PreparseResult::Import(path) => {
                contents.imports.push(path);
            },

            PreparseResult::TypeTemplate(template) => {
                contents.type_templates.insert(template.name.to_string(), ModuleResource::with_visibility(template, data.visibility_mode));
            },

            PreparseResult::FunctionTemplate(template) => {
                contents.function_templates.push(ModuleResource::with_visibility(template, data.visibility_mode));
            },

            PreparseResult::Nothing => {}
        } 
    }
    
    Some(contents)
}

