#[macro_use]
mod log;

use cx_tokens::token::Token;
use cx_util::{CXError, CXResult};
pub use log::LexerError;
use std::path::{Path, PathBuf};

use crate::context::LexingContext;

pub(crate) mod context;
pub(crate) mod lexer;
pub(crate) mod preprocessor;

pub fn lex(source: &str) -> CXResult<Vec<Token>> {
    lex_with_context(source, Path::new("<anonymous>"), &[])
}

pub fn lex_with_context(
    source: &str,
    source_path: &Path,
    include_dirs: &[PathBuf],
) -> CXResult<Vec<Token>> {
    LexingContext::new(source.to_string(), source_path, include_dirs)?.run()
}

pub fn lex_file(source_path: &Path, include_dirs: &[PathBuf]) -> CXResult<Vec<Token>> {
    let source = std::fs::read_to_string(source_path).map_err(|e| {
        CXError::create_boxed(format!(
            "Failed to read source file {}: {}",
            source_path.display(),
            e
        ))
    })?;

    lex_with_context(&source, source_path, include_dirs)
}
