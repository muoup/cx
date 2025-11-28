use cx_lexer_data::token::Token;
use std::path::Path;

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

        acc -= line.len() + 1; // +1 for the newline character
    }

    panic!("Index out of bounds");
}

fn error_exit() -> ! {
    if cfg!(debug_assertions) {
        panic!();
    } else {
        std::process::exit(1);
    }
}

fn annotation_failure(message: &str) -> ! {
    println!("{message}");
    println!("Annotation Failure!");
    error_exit();
}

pub fn pretty_underline_error(
    message: &str,
    file_path: &Path,
    start_index: usize,
    end_index: usize,
) {
    let tokens = cx_lexer::lex(
        &std::fs::read_to_string(file_path)
            .unwrap_or_else(|_| panic!("Failed to read file: {}", file_path.to_string_lossy()))
    );
    
    let Some(tokens) = tokens else { 
        panic!("No tokens provided for error reporting");
    };

    let Some(start_token) = tokens.get(start_index) else {
        annotation_failure(message);
    };
    
    let Some(end_token) = tokens.get(end_index.saturating_sub(1)) else {
        annotation_failure(message);
    };
    
    if start_token.file_origin != end_token.file_origin {
        annotation_failure(message);
    }
    
    let start_index = start_token.start_index;
    let end_index = end_token.end_index;

    let file_contents = std::fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Failed to read file: {}", file_path.to_string_lossy()));

    let (error_line, mut error_padding) = get_error_loc(&file_contents, start_index);

    // This is unsafe and something is wrong, but not getting anything printed is very frustrating.
    let error_line_start = unsafe { start_index.unchecked_sub(error_padding) };
    let mut remaining_error_chars = unsafe { end_index.unchecked_sub(start_index) };

    let link = format!(
        "{}:{}:{}",
        file_path.to_str().unwrap(),
        error_line,
        error_padding + 1
    );
    println!("{message} \n\t--> {link}");

    let mut iter = file_contents[error_line_start..].lines().peekable();

    while let Some(line) = iter.next() {
        // capture the leading whitespace
        let lpad = line_as_spacing(&line[..error_padding.min(line.len())]);
        let underline = "~".repeat((line.len() - error_padding).min(remaining_error_chars));

        println!("{line}");
        println!("{lpad}{underline}");

        error_padding = iter
            .peek()
            .map(|next_line| leading_whitespace_count(next_line))
            .unwrap_or(0);
        remaining_error_chars -= underline.len();

        if remaining_error_chars == 0 {
            break;
        }
    }
    
    error_exit();
}

pub fn pretty_point_error(message: &str, file_path: &Path, token: &Token) {
    let start_index = token.start_index;

    let file_contents = std::fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Failed to read file: {}", file_path.to_string_lossy()));

    let (error_line, error_padding) = get_error_loc(&file_contents, start_index);
    let error_line_start = start_index - error_padding;

    let link = format!(
        "{}:{}:{}",
        file_path.to_str().unwrap(),
        error_line,
        error_padding + 1
    );
    println!("{message} \n\t --> {link}");

    if let Some(line) = file_contents[error_line_start..].lines().next() {
        let lpad = line_as_spacing(&line[..error_padding]);
        println!("{line}");
        println!("{lpad}^");
    }
}
