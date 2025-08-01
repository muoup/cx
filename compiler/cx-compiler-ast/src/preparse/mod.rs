pub mod typing;
pub mod preparser;

mod macros;
mod importing;

use crate::preparse::preparser::{preparse_stmt, PreparseResult};
use cx_data_ast::parse::intrinsic_types::{INTRINSIC_IMPORTS, INTRINSIC_TYPES};
use cx_data_ast::parse::parser::{TokenIter, VisibilityMode};
use cx_data_ast::parse::template::CXTemplateGenerator;
use cx_data_ast::parse::value_type::CXTypeKind;
use cx_data_ast::PreparseContents;
use cx_data_bytecode::mangling::mangle_destructor;
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
                contents.function_definitions.insert(signature.name.to_string(), signature);
            },

            PreparseResult::DestructorDefinition(name) => {
                contents.destructor_definitions.push(name);
            },

            PreparseResult::Import(path) => {
                contents.imports.push(path);
            },

            PreparseResult::TemplateDefinition(name, generator) => {
                match &generator.generator {
                    CXTemplateGenerator::FunctionGen(_) =>
                        contents.function_definitions.insert_template(name, generator),
                    CXTemplateGenerator::TypeGen(_) =>
                        contents.type_definitions.insert_template(name, generator)
                }
            },

            PreparseResult::Nothing => {}
        } 
    }
    
    for destructor in contents.destructor_definitions.iter() {
        let CXTypeKind::Structured { has_destructor, .. } = &mut contents.type_definitions
            .get_mut(&destructor)
            .unwrap_or_else(|| panic!("Destructor defined for unknown type: {}", destructor))
            .kind
        else {
            log_error!("Destructor defined for non-structured type: {}", destructor);
        };
        
        *has_destructor = true;
    }
    
    for (name, _type) in INTRINSIC_TYPES.iter() {
        contents.type_definitions.insert(name.to_string(), _type.clone().to_val_type());
    }
    
    for name in INTRINSIC_IMPORTS.iter() {
        contents.imports.push(name.to_string());
    }

    Some(contents)
}

