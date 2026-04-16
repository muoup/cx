use cx_util::CXErrorTrait;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug)]
pub struct LexerError {
    pub message: String,
    pub file: PathBuf,
    pub source: String,
    pub start_index: usize,
    pub end_index: usize,
}

fn leading_whitespace_count(s: &str) -> usize {
    s.chars().take_while(|c| c.is_whitespace()).count()
}

fn line_as_spacing(line: &str) -> String {
    line.chars()
        .map(|c| if c.is_whitespace() { c } else { ' ' })
        .collect()
}

fn get_error_loc(file_contents: &str, index: usize) -> (usize, usize) {
    let mut acc = index;

    for (line_num, line) in file_contents.lines().enumerate() {
        if line.len() + 1 > acc {
            return (line_num + 1, acc);
        }

        acc -= line.len() + 1;
    }

    let last_line = file_contents.lines().count().max(1);
    let last_col = file_contents
        .lines()
        .last()
        .map(|line| line.len())
        .unwrap_or(0);
    (last_line, last_col)
}

fn pretty_underline_error(
    message: &str,
    file_path: &Path,
    source: &str,
    start_index: usize,
    end_index: usize,
) {
    let (error_line, mut error_padding) = get_error_loc(source, start_index.min(source.len()));
    let error_line_start = start_index.saturating_sub(error_padding).min(source.len());
    let mut remaining_error_chars = end_index.saturating_sub(start_index).max(1);

    let link = format!(
        "{}:{}:{}",
        file_path.to_string_lossy(),
        error_line,
        error_padding + 1
    );
    println!("{message} \n\t--> {link}");

    let mut iter = source[error_line_start..].lines().peekable();

    while let Some(line) = iter.next() {
        let lpad = line_as_spacing(&line[..error_padding.min(line.len())]);
        let underline_width = (line.len().saturating_sub(error_padding))
            .max(1)
            .min(remaining_error_chars);
        let underline = "~".repeat(underline_width);

        println!("{line}");
        println!("{lpad}{underline}");

        remaining_error_chars = remaining_error_chars.saturating_sub(underline_width);
        if remaining_error_chars == 0 {
            break;
        }

        error_padding = iter
            .peek()
            .map(|next_line| leading_whitespace_count(next_line))
            .unwrap_or(0);
    }
}

impl CXErrorTrait for LexerError {
    fn pretty_print(&self) {
        pretty_underline_error(
            &self.error_message(),
            self.file.as_path(),
            &self.source,
            self.start_index,
            self.end_index,
        );
    }

    fn error_prefix(&self) -> String {
        "LEXER ERROR".to_string()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }

    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.file.clone())
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[macro_export]
macro_rules! log_lexer_error {
    ($file:expr, $source:expr, $start:expr, $end:expr, $($arg:tt)*) => {
        {
            Err(Box::new($crate::log::LexerError {
                message: format!($($arg)*),
                file: std::path::PathBuf::from($file),
                source: $source.to_string(),
                start_index: $start,
                end_index: $end,
            }) as Box<dyn cx_util::CXErrorTrait>)
        }
    };
}
