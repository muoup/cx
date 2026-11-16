use std::path::Path;

use crate::format::utils::{
    clamp_to_char_boundary, format_error_link, get_error_loc, leading_whitespace_count,
    line_as_spacing, next_char_boundary,
};

pub(crate) fn pretty_underline_error(
    f: &mut dyn std::io::Write,
    file_path: &Path,
    start_index: usize,
    end_index: usize,
) -> std::io::Result<()> {
    let Some(source) = std::fs::read_to_string(file_path).ok() else {
        writeln!(f, " --> {} (file could not be read)", file_path.display())?;
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

    let mut lines = Vec::new();
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
        lines.push((line, lpad, underline_width));

        if end_index <= line_end || line_end == source.len() {
            break;
        }

        line_start = line_end.saturating_add(1);
    }

    let width = (error_line + lines.len() - 1).to_string().len();
    let gutter = " ".repeat(width);
    let link = format_error_link(file_path, error_line, error_padding);
    writeln!(f, "{gutter}--> {link}")?;
    writeln!(f, "{gutter} |")?;
    for (offset, (line, lpad, underline_width)) in lines.iter().enumerate() {
        writeln!(f, "{:>width$} | {line}", error_line + offset)?;
        writeln!(f, "{gutter} | {lpad}{}", "^".repeat(*underline_width))?;
    }

    Ok(())
}
