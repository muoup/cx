pub mod typing;

mod preparser;
mod macros;
mod importing;

use crate::preparse::preparser::preparse_stmt;
use cx_data_ast::parse::parser::TokenIter;
use cx_data_ast::PreparseContents;

pub fn preparse(tokens: &mut TokenIter) -> Option<PreparseContents> {
    let mut pp_contents = PreparseContents::default();

    while tokens.has_next() {
        preparse_stmt(tokens, &mut pp_contents)?;
    }

    Some(pp_contents)
}

