use std::path::{Path, PathBuf};

use cx_log::{
    CXResult,
    error::{CXErr, context::CXPointingContext, message::CXStdErrMessage},
};
use cx_util::char_iter::CharIter;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum LanguageMode {
    C,
    Cx,
}

impl LanguageMode {
    pub(crate) fn for_root_path(path: &Path) -> Self {
        match path.extension().and_then(|extension| extension.to_str()) {
            Some("c" | "h") => Self::C,
            _ => Self::Cx,
        }
    }

    pub(crate) fn for_include_path(path: &Path, parent: Self, angled: bool) -> Self {
        match path.extension().and_then(|extension| extension.to_str()) {
            Some("c") => Self::C,
            Some("cx" | "cxh") => Self::Cx,
            Some("h") if angled => Self::C,
            _ => parent,
        }
    }
}

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
    pub(crate) is_include: bool,
    pub(crate) language_mode: LanguageMode,
}

impl SourceFrame {
    pub(crate) fn new(source: String, source_path: &Path) -> Self {
        Self::new_with_mode(
            source,
            source_path,
            LanguageMode::for_root_path(source_path),
        )
    }

    fn new_with_mode(source: String, source_path: &Path, language_mode: LanguageMode) -> Self {
        Self {
            source,
            file_path: source_path.to_path_buf(),
            cursor: 0,
            conditionals: Vec::new(),
            is_include: false,
            language_mode,
        }
    }

    pub(crate) fn new_include(
        source: String,
        source_path: &Path,
        language_mode: LanguageMode,
    ) -> Self {
        let mut frame = Self::new_with_mode(source, source_path, language_mode);
        frame.is_include = true;
        frame
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

    pub(crate) fn cursor_view(&self) -> LexCursor<'_> {
        LexCursor::new(&self.source, self.file_path.as_path(), self.cursor)
    }

    pub(crate) fn with_cursor<T>(&mut self, f: impl FnOnce(&mut LexCursor<'_>) -> T) -> T {
        let mut cursor = self.cursor_view();
        let result = f(&mut cursor);
        self.cursor = cursor.cursor();
        result
    }

    pub(crate) fn skip_whitespace(&mut self) {
        self.with_cursor(|cursor| cursor.skip_whitespace());
    }

    pub(crate) fn next_word(&mut self) -> Option<String> {
        self.with_cursor(|cursor| cursor.next_word().map(str::to_string))
    }

    pub(crate) fn rest_of_line(&mut self) -> String {
        self.with_cursor(|cursor| cursor.rest_of_line().to_string())
    }
}

pub(crate) struct LexCursor<'a> {
    file_path: &'a Path,
    iter: CharIter<'a>,
}

impl<'a> LexCursor<'a> {
    pub(crate) fn new(source: &'a str, file_path: &'a Path, byte_index: usize) -> Self {
        let mut iter = CharIter::new(source);
        iter.current_iter = byte_index;

        Self { file_path, iter }
    }

    pub(crate) fn log_error<T>(
        &self,
        byte_index: usize,
        message: impl Into<String>,
    ) -> CXResult<T> {
        Err(CXErr::new(
            CXStdErrMessage::error("LEXER ERROR", message),
            CXPointingContext::error(self.file_path.to_path_buf(), byte_index),
        ))
    }

    pub(crate) fn source(&self) -> &'a str {
        self.iter.source
    }

    pub(crate) fn cursor(&self) -> usize {
        self.iter.current_iter
    }

    pub(crate) fn has_next(&self) -> bool {
        self.iter.has_next()
    }

    pub(crate) fn peek(&self) -> Option<char> {
        self.iter.peek()
    }

    pub(crate) fn next(&mut self) -> Option<char> {
        self.iter.next()
    }

    pub(crate) fn next_is<T>(&self, condition: T) -> bool
    where
        T: Fn(&u8) -> bool,
    {
        self.iter.next_is(condition)
    }

    pub(crate) fn back(&mut self) {
        self.iter.back();
    }

    pub(crate) fn skip_whitespace(&mut self) {
        self.iter.skip_whitespace();
    }

    pub(crate) fn skip_line(&mut self) {
        self.iter.skip_line();
    }

    pub(crate) fn next_word(&mut self) -> Option<&str> {
        self.iter.next_word()
    }

    pub(crate) fn rest_of_line(&mut self) -> &str {
        self.iter.rest_of_line()
    }
}

#[cfg(test)]
mod tests {
    use super::LanguageMode;
    use std::path::Path;

    #[test]
    fn language_mode_follows_source_extensions() {
        assert_eq!(
            LanguageMode::for_root_path(Path::new("main.c")),
            LanguageMode::C
        );
        assert_eq!(
            LanguageMode::for_root_path(Path::new("main.cx")),
            LanguageMode::Cx
        );
        assert_eq!(
            LanguageMode::for_include_path(Path::new("api.h"), LanguageMode::Cx, true),
            LanguageMode::C
        );
        assert_eq!(
            LanguageMode::for_include_path(Path::new("local.h"), LanguageMode::Cx, false),
            LanguageMode::Cx
        );
        assert_eq!(
            LanguageMode::for_include_path(Path::new("tokens.inc"), LanguageMode::C, false),
            LanguageMode::C
        );
    }
}
