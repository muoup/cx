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
            todo!()
        }
    };
}
