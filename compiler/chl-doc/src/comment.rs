// The run of '///' lines directly above the line a declaration starts on
pub fn leading(source: &str, start: usize) -> Option<String> {
    let line_start = source.get(..start)?.rfind('\n').map_or(0, |index| index + 1);
    let mut lines = source[..line_start]
        .lines()
        .rev()
        .map_while(|line| marked(line, "///"))
        .collect::<Vec<_>>();
    lines.reverse();
    joined(lines)
}

// The run of '//!' lines that opens a file
pub fn module(source: &str) -> Option<String> {
    let lines = source
        .lines()
        .skip_while(|line| line.trim().is_empty())
        .map_while(|line| marked(line, "//!"))
        .collect();
    joined(lines)
}

fn marked<'a>(line: &'a str, marker: &str) -> Option<&'a str> {
    let text = line.trim_start().strip_prefix(marker)?;
    if text.starts_with('/') {
        return None;
    }
    Some(text.strip_prefix(' ').unwrap_or(text).trim_end())
}

fn joined(lines: Vec<&str>) -> Option<String> {
    let text = lines.join("\n");
    let text = text.trim_matches('\n');
    (!text.trim().is_empty()).then(|| text.to_string())
}