pub mod typing;
pub mod preparser;

mod macros;
mod importing;

use crate::preparse::preparser::preparse_stmt;
use cx_data_ast::parse::intrinsic_types::{INTRINSIC_IMPORTS, INTRINSIC_TYPES};
use cx_data_ast::parse::parser::TokenIter;
use cx_data_ast::parse::value_type::CXTypeKind;
use cx_data_ast::PreparseContents;
use cx_util::{log_error, point_log_error};

pub(crate) struct PreparseData<'a> {
    tokens: TokenIter<'a>,
    destructors: Vec<String>,
    template_definition: bool,
}

pub fn preparse(mut tokens: TokenIter) -> Option<PreparseContents> {
    tokens.reset();
    
    let mut data = PreparseData {
        tokens: tokens,
        destructors: Vec::new(),
        template_definition: false
    };
    let mut pp_contents = PreparseContents::default();

    while data.tokens.has_next() {
        let Some(_) = preparse_stmt(&mut data, &mut pp_contents) else {
            point_log_error!(data.tokens, "Failed to preparse statement")
        };
    }
    
    for destructor in data.destructors {
        let CXTypeKind::Structured { has_destructor, .. } = &mut pp_contents.type_definitions
            .get_mut(&destructor)
            .unwrap_or_else(|| panic!("Destructor defined for unknown type: {}", destructor))
            .kind
        else {
            log_error!("Destructor defined for non-structured type: {}", destructor);
        };
        
        *has_destructor = true;
    }
    
    for (name, _type) in INTRINSIC_TYPES.iter() {
        pp_contents.type_definitions.insert(name.to_string(), _type.clone().to_val_type());
    }
    
    for name in INTRINSIC_IMPORTS.iter() {
        pp_contents.imports.push(name.to_string());
    }

    Some(pp_contents)
}

