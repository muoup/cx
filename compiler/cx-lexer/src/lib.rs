#[macro_use]
mod log;

use crate::unified_lexer::Lexer;
use cx_tokens::token::Token;
use cx_util::CXResult;
pub use log::LexerError;
use std::path::{Path, PathBuf};

pub(crate) mod line_lexer;
pub(crate) mod preprocessor;
pub(crate) mod unified_lexer;
pub(crate) mod new_lexer;

pub fn lex(source: &str) -> CXResult<Vec<Token>> {
    lex_with_context(source, Path::new("<anonymous>"), &[])
}

pub fn lex_with_context(
    source: &str,
    source_path: &Path,
    include_dirs: &[PathBuf],
) -> CXResult<Vec<Token>> {
    let mut lexer = Lexer::new(source, source_path, include_dirs);
    lexer.lex_source()?;

    Ok(lexer.tokens)
}
