use std::path::Path;
use cx_data_lexer::token::Token;

fn get_error_loc(file_contents: &str, index: usize) -> (usize, usize) {
    let mut acc = index;

    for (line_num, line) in file_contents.lines().enumerate() {
        if line.len() + 1 > acc {
            return (line_num + 1, acc - 1);
        }

        acc -= line.len() + 1; // +1 for the newline character
    }

    panic!("Index out of bounds");
}

pub fn pretty_underline_error(message: &str, file_path: &Path, tokens: &[Token], start_index: usize, end_index: usize) {
    if tokens.is_empty() {
        panic!("No tokens provided for error reporting");
    }

    let start_index = tokens[start_index].start_index;
    let end_index = tokens[end_index].end_index;

    let file_contents = std::fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Failed to read file: {}", file_path.to_string_lossy()));

    let (error_line, mut error_padding) = get_error_loc(&file_contents, start_index);

    let error_line_start = start_index - error_padding - 1;
    let mut remaining_error_chars = end_index - start_index;

    let link = format!("{} {}:{}", file_path.to_str().unwrap(), error_line, error_padding);
    println!("{}\n\t--> {}", message, link);

    for line in file_contents[error_line_start..].lines() {
        let lpad = " ".repeat(error_padding);
        let underline = "~".repeat((line.len() - error_padding).min(remaining_error_chars));

        println!("{}", line);
        println!("{}{}", lpad, underline);

        error_padding = 0;
        remaining_error_chars -= underline.len();

        if remaining_error_chars <= 0 {
            break;
        }
    }

    std::process::exit(1);
}