use std::{io::Write, path::Path};

pub(crate) fn write_unspanned(
    f: &mut dyn Write,
    message: &str,
    notes: &[String],
) -> std::io::Result<()> {
    writeln!(f, "{message}")?;
    for note in notes {
        writeln!(f, "note: {note}")?;
    }

    Ok(())
}

pub(crate) fn pretty_underline_error(
    f: &mut dyn Write,
    message: &str,
    notes: &[String],
    file_path: &Path,
    start_index: usize,
    end_index: usize,
) -> std::io::Result<()> {
    let Some(source) = std::fs::read_to_string(file_path).ok() else {
        writeln!(f, "{} (File could not be read)", message)?;
        return Ok(());
    };

    let start_index = clamp_to_char_boundary(source.as_str(), start_index.min(source.len()));
    let end_index = clamp_to_char_boundary(
        source.as_str(),
        end_index
            .max(start_index.saturating_add(1))
            .min(source.len()),
    );
    let (error_line, error_padding) = get_error_loc(source.as_str(), start_index);
    let first_line_start = start_index.saturating_sub(error_padding);

    let link = format_error_link(file_path, error_line, error_padding);
    writeln!(f, "{message} \n\t--> {link}")?;
    for note in notes {
        writeln!(f, "note: {note}")?;
    }

    let mut line_start = first_line_start;
    loop {
        let line_end = source[line_start..]
            .find('\n')
            .map(|offset| line_start + offset)
            .unwrap_or(source.len());
        let line = &source[line_start..line_end];
        let underline_start = if line_start == first_line_start {
            start_index.saturating_sub(line_start)
        } else {
            leading_whitespace_count(line)
        }
        .min(line.len());
        let mut underline_end = end_index.min(line_end).saturating_sub(line_start);
        if underline_end <= underline_start {
            underline_end =
                next_char_boundary(line, underline_start).unwrap_or(underline_start + 1);
        }
        let underline_width = underline_end.saturating_sub(underline_start).max(1);
        let lpad = line_as_spacing(&line[..underline_start.min(line.len())]);

        writeln!(f, "{line}")?;
        writeln!(f, "{lpad}{}", "~".repeat(underline_width))?;

        if end_index <= line_end || line_end == source.len() {
            break;
        }

        line_start = line_end.saturating_add(1);
    }

    Ok(())
}

pub fn point_error(
    f: &mut dyn Write,
    message: &str,
    notes: &[String],
    file_path: &Path,
    index: usize,
) -> std::io::Result<()> {
    let Some(source) = std::fs::read_to_string(file_path).ok() else {
        writeln!(f, "{} (File could not be read)", message)?;
        return Ok(());
    };

    let start_index = clamp_to_char_boundary(source.as_str(), index.min(source.len()));
    let (error_line, error_padding) = get_error_loc(source.as_str(), start_index);
    let error_line_start = start_index - error_padding;

    let link = format_error_link(file_path, error_line, error_padding);

    writeln!(f, "{message} \n\t --> {link}")?;

    for note in notes {
        writeln!(f, "note: {note}")?;
    }

    if let Some(line) = source[error_line_start..].lines().next() {
        let lpad = line_as_spacing(&line[..error_padding]);
        writeln!(f, "{line}")?;
        writeln!(f, "{lpad}^")?;
    }

    Ok(())
}

pub fn leading_whitespace_count(s: &str) -> usize {
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

fn format_error_link(file_path: &Path, error_line: usize, error_padding: usize) -> String {
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

fn clamp_to_char_boundary(source: &str, mut index: usize) -> usize {
    while index > 0 && !source.is_char_boundary(index) {
        index -= 1;
    }
    index
}

fn next_char_boundary(source: &str, index: usize) -> Option<usize> {
    if index >= source.len() {
        return None;
    }

    source[index..]
        .chars()
        .next()
        .map(|ch| index + ch.len_utf8())
}
