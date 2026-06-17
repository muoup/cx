use std::path::Path;

use cx_tokens::byte_range_for_tokens;

pub fn byte_range_for_source_tokens(
    file_path: &Path,
    start_token: usize,
    end_token: usize,
) -> (usize, usize) {
    let Ok(source) = std::fs::read_to_string(file_path) else {
        return (0, 1);
    };
    let Ok(tokens) = cx_lexer::lex(&source) else {
        return (0, 1);
    };

    byte_range_for_tokens(&tokens, start_token, end_token)
}

#[macro_export]
macro_rules! log_analysis_error {
    ($env:expr, $expr:expr, $($arg:tt)*) => {
        {
            let message = format!("{}", format!($($arg)*));

            let (token_start, token_end) = if let Some(token) = $expr.token_range.as_ref() {
                (token.start_token, token.end_token)
            } else {
                (0, 0) // Default to 0 if no token information is available
            };
            let range_file = $expr.token_range.as_ref().and_then(|range| {
                (!range.file_origin.is_empty()).then_some(std::path::PathBuf::from(range.file_origin.as_ref()))
            });
            let compilation_unit = range_file
                .as_ref()
                .unwrap_or(&$env.compilation_unit)
                .to_owned();
            let (byte_start, byte_end) =
                $crate::log::byte_range_for_source_tokens(compilation_unit.as_path(), token_start, token_end);

            let span = cx_log::DiagnosticSpan::new(compilation_unit, byte_start, byte_end);

            Err(Box::new(
                cx_log::UnderlineError::new("ANALYSIS ERROR", message, span),
            ) as Box<dyn cx_log::CXErrorTrait>)
        }
    };
}
