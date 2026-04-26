use std::path::{Path, PathBuf};

use cx_util::char_iter::CharIter;

#[derive(Clone, Debug)]
pub(crate) struct ConditionalFrame {
    pub(crate) parent_active: bool,
    pub(crate) branch_active: bool,
    pub(crate) any_branch_taken: bool,
    pub(crate) else_seen: bool,
}

pub(crate) struct SourceFrame {
    pub(crate) source: String,
    pub(crate) file_path: PathBuf,
    pub(crate) cursor: usize,
    pub(crate) conditionals: Vec<ConditionalFrame>,
}

impl SourceFrame {
    pub(crate) fn new(source: String, source_path: &Path) -> Self {
        Self {
            source,
            file_path: source_path.to_path_buf(),
            cursor: 0,
            conditionals: Vec::new(),
        }
    }

    pub(crate) fn is_active(&self) -> bool {
        self.conditionals
            .last()
            .map(|frame| frame.branch_active)
            .unwrap_or(true)
    }

    pub(crate) fn has_next(&self) -> bool {
        self.cursor < self.source.len()
    }

    pub(crate) fn peek(&self) -> Option<char> {
        self.source.as_bytes().get(self.cursor).map(|&c| c as char)
    }

    pub(crate) fn with_iter<T>(&mut self, f: impl FnOnce(&mut CharIter<'_>) -> T) -> T {
        let mut iter = CharIter::new(&self.source);
        iter.current_iter = self.cursor;
        let result = f(&mut iter);
        self.cursor = iter.current_iter;
        result
    }

    pub(crate) fn skip_whitespace(&mut self) {
        self.with_iter(|iter| iter.skip_whitespace());
    }

    pub(crate) fn next_word(&mut self) -> Option<String> {
        self.with_iter(|iter| iter.next_word().map(str::to_string))
    }

    pub(crate) fn rest_of_line(&mut self) -> String {
        self.with_iter(|iter| iter.rest_of_line().to_string())
    }
}
