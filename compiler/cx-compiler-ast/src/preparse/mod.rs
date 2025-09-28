pub mod typing;
pub mod preparser;

mod macros;
mod importing;

use crate::preparse::preparser::{preparse_stmt, PreparseResult};
use cx_data_ast::parse::parser::VisibilityMode;
use cx_data_ast::preparse::naive_types::ModuleResource;
use cx_data_ast::PreparseContents;
use cx_data_lexer::TokenIter;
use cx_util::{log_error, point_log_error};
use crate::parse::global_scope::destructor_prototype;

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
                contents.type_definitions.insert_standard(name, ModuleResource::with_visibility(type_, data.visibility_mode));
            },

            PreparseResult::FunctionDefinition(signature) => {
                contents.function_definitions.insert_standard(
                    signature.name.mangle(),
                    ModuleResource::with_visibility(signature, data.visibility_mode)
                );
            },

            PreparseResult::DestructorDefinition(_type) => {
                let prototype = destructor_prototype(_type);

                contents.function_definitions.insert_standard(
                    prototype.name.mangle(),
                    ModuleResource::with_visibility(prototype, data.visibility_mode)
                );
            }

            PreparseResult::Import(path) => {
                contents.imports.push(path);
            },

            PreparseResult::TypeTemplate(template) => {
                contents.type_definitions.insert_template(template.name.to_string(), ModuleResource::with_visibility(template, data.visibility_mode));
            },

            PreparseResult::FunctionTemplate(template) => {
                contents.function_definitions.insert_template(
                    template.name.to_string(),
                    ModuleResource::with_visibility(template, data.visibility_mode)
                );
            },

            PreparseResult::Nothing => {}
        } 
    }
    
    Some(contents)
}

