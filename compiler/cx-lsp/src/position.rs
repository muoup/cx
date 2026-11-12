use rustc_hash::FxHashMap;
use std::path::{Path, PathBuf};
use tower_lsp::lsp_types::{Position, Range};

pub struct SourceText {
    text: String,
    line_starts: Vec<usize>,
}

impl SourceText {
    pub fn new(text: String) -> Self {
        let line_starts = std::iter::once(0)
            .chain(text.match_indices('\n').map(|(index, _)| index + 1))
            .collect();
        Self { text, line_starts }
    }

    pub fn position(&self, index: usize) -> Position {
        let index = self.floor_char_boundary(index);
        let line = self.line_starts.partition_point(|start| *start <= index) - 1;
        let prefix = &self.text[self.line_starts[line]..index];
        let prefix = prefix.strip_suffix('\r').unwrap_or(prefix);

        Position {
            line: line as u32,
            character: prefix.encode_utf16().count() as u32,
        }
    }

    pub fn range(&self, start: usize, end: usize) -> Range {
        let start = self.floor_char_boundary(start);
        let end = if end <= start {
            self.text[start..]
                .chars()
                .next()
                .map_or(start, |character| start + character.len_utf8())
        } else {
            self.floor_char_boundary(end)
        };

        Range {
            start: self.position(start),
            end: self.position(end),
        }
    }

    pub fn slice(&self, start: usize, end: usize) -> &str {
        let start = self.floor_char_boundary(start);
        let end = self.floor_char_boundary(end).max(start);
        &self.text[start..end]
    }

    pub fn line(&self, one_based_line: Option<usize>) -> Range {
        let line = one_based_line
            .unwrap_or(1)
            .saturating_sub(1)
            .min(self.line_starts.len() - 1);
        let start = self.line_starts[line];
        let end = self
            .line_starts
            .get(line + 1)
            .map_or(self.text.len(), |next| next - 1);

        Range {
            start: self.position(start),
            end: self.position(end),
        }
    }

    fn floor_char_boundary(&self, index: usize) -> usize {
        let mut index = index.min(self.text.len());
        while !self.text.is_char_boundary(index) {
            index -= 1;
        }
        index
    }
}

#[derive(Default)]
pub struct SourceCache {
    files: FxHashMap<PathBuf, Option<SourceText>>,
}

impl SourceCache {
    pub fn get(&mut self, path: &Path) -> Option<&SourceText> {
        self.files
            .entry(path.to_path_buf())
            .or_insert_with(|| std::fs::read_to_string(path).ok().map(SourceText::new))
            .as_ref()
    }
}
