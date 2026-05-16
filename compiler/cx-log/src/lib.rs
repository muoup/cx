use std::path::Path;

fn leading_whitespace_count(s: &str) -> usize {
    s.char_indices()
        .take_while(|(_, c)| c.is_whitespace())
        .map(|(index, c)| index + c.len_utf8())
        .last()
        .unwrap_or(0)
}

fn line_as_spacing(line: &str) -> String {
    line.chars()
        .map(|c| if c.is_whitespace() { c } else { ' ' })
        .collect()
}

pub fn get_error_loc(file_contents: &str, index: usize) -> (usize, usize) {
    let mut acc = index.min(file_contents.len());

    for (line_num, line) in file_contents.lines().enumerate() {
        if line.len() + 1 > acc {
            return (line_num + 1, acc);
        }

        acc -= line.len() + 1; // +1 for the newline character
    }

    let last_line = file_contents.lines().count().max(1);
    let last_col = file_contents
        .lines()
        .last()
        .map(|line| line.len())
        .unwrap_or(0);
    (last_line, last_col)
}

fn error_exit() {}

pub fn pretty_underline_error(
    message: &str,
    file_path: &Path,
    start_index: usize,
    end_index: usize,
) {
    pretty_underline_error_with_notes(message, &[], file_path, start_index, end_index);
}

pub fn pretty_underline_error_with_notes(
    message: &str,
    notes: &[String],
    file_path: &Path,
    start_index: usize,
    end_index: usize,
) {
    let Some(file_contents) = std::fs::read_to_string(file_path).ok() else {
        println!("{} (File could not be read)", message);
        return;
    };

    pretty_underline_source_error_with_notes(
        message,
        notes,
        file_path,
        &file_contents,
        start_index,
        end_index,
    );
}

pub fn pretty_underline_source_error(
    message: &str,
    file_path: &Path,
    source: &str,
    start_index: usize,
    end_index: usize,
) {
    pretty_underline_source_error_with_notes(
        message,
        &[],
        file_path,
        source,
        start_index,
        end_index,
    );
}

pub fn pretty_underline_source_error_with_notes(
    message: &str,
    notes: &[String],
    file_path: &Path,
    source: &str,
    start_index: usize,
    end_index: usize,
) {
    let start_index = clamp_to_char_boundary(source, start_index.min(source.len()));
    let end_index = clamp_to_char_boundary(source, end_index.min(source.len()));
    let (error_line, mut error_padding) = get_error_loc(source, start_index);
    let error_line_start = start_index.saturating_sub(error_padding);
    let mut remaining_error_chars = end_index.saturating_sub(start_index).max(1);

    let link = format!(
        "{}:{}:{}",
        file_path.to_string_lossy(),
        error_line,
        error_padding + 1
    );
    println!("{message} \n\t--> {link}");
    for note in notes {
        println!("note: {note}");
    }

    let mut iter = source[error_line_start..].lines().peekable();

    while let Some(line) = iter.next() {
        // capture the leading whitespace
        let lpad = line_as_spacing(&line[..error_padding.min(line.len())]);
        let underline_width = line
            .len()
            .saturating_sub(error_padding)
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

    error_exit();
}

pub fn pretty_point_error(message: &str, file_path: &Path, index: usize) {
    let file_contents = std::fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Failed to read file: {}", file_path.to_string_lossy()));

    pretty_point_source_error(message, file_path, &file_contents, index);
}

pub fn pretty_point_source_error(message: &str, file_path: &Path, source: &str, index: usize) {
    let start_index = clamp_to_char_boundary(source, index.min(source.len()));
    let (error_line, error_padding) = get_error_loc(source, start_index);
    let error_line_start = start_index - error_padding;

    let link = format!(
        "{}:{}:{}",
        file_path.to_string_lossy(),
        error_line,
        error_padding + 1
    );
    println!("{message} \n\t --> {link}");

    if let Some(line) = source[error_line_start..].lines().next() {
        let lpad = line_as_spacing(&line[..error_padding]);
        println!("{line}");
        println!("{lpad}^");
    }
}

fn clamp_to_char_boundary(source: &str, mut index: usize) -> usize {
    while index > 0 && !source.is_char_boundary(index) {
        index -= 1;
    }
    index
}
