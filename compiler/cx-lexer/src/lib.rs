use cx_log::CXResult;
use cx_log::catalogue::parse::READ_FILE;
use cx_tokens::token::Token;
use std::path::Path;

use crate::context::LexingContext;

pub use include_root::{IncludeRoot, IncludeSource};

pub(crate) mod context;
mod include_root;
pub(crate) mod lexer;
pub(crate) mod log;
pub(crate) mod preprocessor;

pub fn lex(source: &str) -> CXResult<Vec<Token>> {
    lex_with_context(source, Path::new("<anonymous>"), &[], &[])
}

pub fn lex_with_context(
    source: &str,
    source_path: &Path,
    includes: &[IncludeRoot],
    predefined_macros: &[(String, String)],
) -> CXResult<Vec<Token>> {
    LexingContext::new(
        source.to_string(),
        source_path,
        includes,
        predefined_macros,
    )?
    .run()
}

pub fn lex_file(source_path: &Path, includes: &[IncludeRoot]) -> CXResult<Vec<Token>> {
    let source = std::fs::read_to_string(source_path).map_err(|e| {
        crate::log::no_source_error(
            &READ_FILE,
            (
                "source file".into(),
                source_path.display().to_string(),
                e.to_string(),
            ),
            "failed to read lexer source file",
        )
    })?;

    lex_with_context(&source, source_path, includes, &[])
}
