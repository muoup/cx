use tower_lsp::lsp_types::{Position, Range};

fn floor_char_boundary(text: &str, index: usize) -> usize {
    let mut index = index.min(text.len());
    while !text.is_char_boundary(index) {
        index -= 1;
    }
    index
}

pub fn byte_index_to_position(text: &str, index: usize) -> Position {
    let index = floor_char_boundary(text, index);
    let prefix = &text[..index];
    let line_start = prefix.rfind('\n').map_or(0, |newline| newline + 1);
    let line_prefix = prefix[line_start..]
        .strip_suffix('\r')
        .unwrap_or(&prefix[line_start..]);

    Position {
        line: prefix.bytes().filter(|byte| *byte == b'\n').count() as u32,
        character: line_prefix.encode_utf16().count() as u32,
    }
}

pub fn byte_range(text: &str, start: usize, end: usize) -> Range {
    let start = floor_char_boundary(text, start);
    let end = if end <= start {
        text[start..]
            .chars()
            .next()
            .map_or(start, |character| start + character.len_utf8())
    } else {
        floor_char_boundary(text, end)
    };

    Range {
        start: byte_index_to_position(text, start),
        end: byte_index_to_position(text, end),
    }
}

pub fn line_range(text: &str, one_based_line: Option<usize>) -> Range {
    let lines = text.split('\n').collect::<Vec<_>>();
    let line = one_based_line
        .unwrap_or(1)
        .saturating_sub(1)
        .min(lines.len().saturating_sub(1));
    let content = lines
        .get(line)
        .copied()
        .unwrap_or_default()
        .strip_suffix('\r')
        .unwrap_or_else(|| lines.get(line).copied().unwrap_or_default());

    Range {
        start: Position {
            line: line as u32,
            character: 0,
        },
        end: Position {
            line: line as u32,
            character: content.encode_utf16().count() as u32,
        },
    }
}
