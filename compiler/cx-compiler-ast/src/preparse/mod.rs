pub mod typing;
pub mod preparser;

mod macros;
mod importing;

use crate::preparse::preparser::preparse_stmt;
use cx_data_ast::parse::intrinsic_types::INTRINSIC_TYPES;
use cx_data_ast::parse::parser::TokenIter;
use cx_data_ast::PreparseContents;

pub(crate) struct PreparseData<'a> {
    tokens: TokenIter<'a>,
    template_definition: bool
}

pub fn preparse(tokens: TokenIter) -> Option<PreparseContents> {
    let mut data = PreparseData {
        tokens: tokens.clone(),
        template_definition: false
    };
    let mut pp_contents = PreparseContents::default();

    while data.tokens.has_next() {
        preparse_stmt(&mut data, &mut pp_contents)?;
    }
    
    for (name, _type) in INTRINSIC_TYPES.iter() {
        pp_contents.type_definitions.insert(name.to_string(), _type.clone().to_val_type());
    }

    Some(pp_contents)
}

