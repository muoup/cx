use cx_lexer_data::TokenIter;
use cx_parsing_data::{parse::parser::VisibilityMode, PreparseContents};

use crate::declarations::decl_parsing::preparse_decl_stmt;

pub(crate) struct PreparseData<'a> {
    pub(crate) contents: &'a mut PreparseContents,
    pub(crate) tokens: TokenIter<'a>,
    pub(crate) visibility_mode: VisibilityMode,
}

pub fn preparse(tokens: TokenIter) -> Option<PreparseContents> {
    let mut contents = PreparseContents::default();

    let mut data = PreparseData {
        contents: &mut contents,
        tokens,
        visibility_mode: VisibilityMode::Private,
    };

    while data.tokens.has_next() {
        let Some(stmt) = preparse_decl_stmt(&mut data.tokens) else {
            log_preparse_error!(data.tokens, "Failed to preparse statement")
        };

        stmt.add_to(&mut data);
    }

    Some(contents)
}
