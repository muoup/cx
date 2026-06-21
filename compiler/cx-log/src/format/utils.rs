use std::path::Path;

pub(crate) fn leading_whitespace_count(s: &str) -> usize {
    s.char_indices()
        .take_while(|(_, c)| c.is_whitespace())
        .map(|(index, c)| index + c.len_utf8())
        .last()
        .unwrap_or(0)
}

pub(crate) fn line_as_spacing(line: &str) -> String {
    line.chars()
        .map(|c| if c.is_whitespace() { c } else { ' ' })
        .collect()
}

pub(crate) fn get_error_loc(file_contents: &str, index: usize) -> (usize, usize) {
    let mut acc = index.min(file_contents.len());

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

pub(crate) fn format_error_link(
    file_path: &Path,
    error_line: usize,
    error_padding: usize,
) -> String {
    format!(
        "{}:{}:{}",
        file_path
            .canonicalize()
            .map(|s| s.to_string_lossy().as_ref().to_owned())
            .unwrap_or("path parse failure".into()),
        error_line,
        error_padding + 1
    )
}

pub(crate) fn clamp_to_char_boundary(source: &str, mut index: usize) -> usize {
    while index > 0 && !source.is_char_boundary(index) {
        index -= 1;
    }
    index
}

pub(crate) fn next_char_boundary(source: &str, index: usize) -> Option<usize> {
    if index >= source.len() {
        return None;
    }

    source[index..]
        .chars()
        .next()
        .map(|ch| index + ch.len_utf8())
}
