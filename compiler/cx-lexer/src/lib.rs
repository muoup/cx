use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_tokens::token::Token;
use std::path::{Path, PathBuf};

use crate::context::LexingContext;

pub(crate) mod context;
pub(crate) mod lexer;
pub(crate) mod preprocessor;

pub fn lex(source: &str) -> CXResult<Vec<Token>> {
    lex_with_context(source, Path::new("<anonymous>"), &[], &[])
}

pub fn lex_with_context(
    source: &str,
    source_path: &Path,
    include_dirs: &[PathBuf],
    predefined_macros: &[(String, String)],
) -> CXResult<Vec<Token>> {
    LexingContext::new(
        source.to_string(),
        source_path,
        include_dirs,
        predefined_macros,
    )?
    .run()
}

pub fn lex_file(source_path: &Path, include_dirs: &[PathBuf]) -> CXResult<Vec<Token>> {
    let source = std::fs::read_to_string(source_path).map_err(|e| {
        CXErr::new(
            CXStdErrMessage::error(
                "LEXER ERROR",
                format!(
                    "Failed to read source file {}: {}",
                    source_path.display(),
                    e
                ),
            ),
            CXInternalContext::error("failed to read lexer source file"),
        )
    })?;

    lex_with_context(&source, source_path, include_dirs, &[])
}

#[cfg(test)]
mod tests {
    use super::lex_with_context;
    use cx_tokens::token::TokenKind;
    use std::path::Path;

    #[test]
    fn expands_predefined_macros_in_conditionals() {
        let source = "#ifdef FLAG\nint value;\n#endif\n";
        let tokens = match lex_with_context(
            source,
            Path::new("main.c"),
            &[],
            &[("FLAG".to_string(), "1".to_string())],
        ) {
            Ok(tokens) => tokens,
            Err(error) => {
                error.print().unwrap();
                panic!("predefined macro lexing failed");
            }
        };

        assert!(tokens.iter().any(|token| {
            matches!(&token.kind, TokenKind::Identifier(name) if name == "value")
        }));
    }
}
