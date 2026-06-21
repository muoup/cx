use std::path::Path;

use crate::format::utils::{
    clamp_to_char_boundary, format_error_link, get_error_loc, line_as_spacing,
};

pub fn point_error(
    f: &mut dyn std::io::Write,
    file_path: &Path,
    index: usize,
) -> std::io::Result<()> {
    let Some(source) = std::fs::read_to_string(file_path).ok() else {
        writeln!(f, "(File could not be read)")?;
        return Ok(());
    };

    let start_index = clamp_to_char_boundary(source.as_str(), index.min(source.len()));
    let (error_line, error_padding) = get_error_loc(source.as_str(), start_index);
    let error_line_start = start_index - error_padding;

    let link = format_error_link(file_path, error_line, error_padding);

    writeln!(f, "\n\t --> {link}")?;

    if let Some(line) = source[error_line_start..].lines().next() {
        let lpad = line_as_spacing(&line[..error_padding]);
        writeln!(f, "{line}")?;
        writeln!(f, "{lpad}^")?;
    }

    Ok(())
}
