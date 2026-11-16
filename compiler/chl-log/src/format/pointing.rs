use std::path::Path;

use crate::format::underline::pretty_underline_error;

pub fn point_error(
    f: &mut dyn std::io::Write,
    file_path: &Path,
    index: usize,
) -> std::io::Result<()> {
    pretty_underline_error(f, file_path, index, index.saturating_add(1))
}
