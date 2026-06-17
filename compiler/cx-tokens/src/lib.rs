use cx_log::{CXResult, DiagnosticPointer};

use crate::token::{PunctuatorType, Token, TokenKind};
pub use crate::token::{TokenRange, TokenRangeArg};
use std::path::PathBuf;

pub mod format;
pub mod token;

#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
    pub slice: &'a [Token],
    pub index: usize,
    pub file: PathBuf,
}

impl<'a> TokenIter<'a> {
    pub fn new(slice: &'a [Token], file: PathBuf) -> Self {
        TokenIter {
            slice,
            index: 0,
            file,
        }
    }

    pub fn next(&mut self) -> Option<&Token> {
        let next = self.slice.get(self.index)?;
        self.index += 1;
        Some(next)
    }

    pub fn peek(&self) -> Option<&Token> {
        self.slice.get(self.index)
    }

    pub fn peek_prev(&self) -> Option<&Token> {
        if self.index == 0 {
            return None;
        }
        self.slice.get(self.index - 1)
    }

    pub fn back(&mut self) {
        self.index -= 1;
    }

    pub fn prev(&self) -> Option<&Token> {
        if self.index == 0 {
            return None;
        }
        self.slice.get(self.index - 1)
    }

    pub fn reset(&mut self) {
        self.index = 0;
    }

    pub fn has_next(&self) -> bool {
        self.slice.get(self.index).is_some()
    }

    pub fn with_index(&mut self, index: usize) -> Self {
        TokenIter {
            slice: self.slice,
            index,
            file: self.file.clone(),
        }
    }

    pub fn goto_statement_end(&mut self) -> CXResult<()> {
        let mut bracket_stack = 0;

        while let Some(token) = self.next() {
            match token.kind {
                TokenKind::Punctuator(PunctuatorType::OpenBrace) => bracket_stack += 1,
                TokenKind::Punctuator(PunctuatorType::CloseBrace) => {
                    bracket_stack -= 1;

                    if bracket_stack == 0 {
                        if matches!(self.peek(), Some(t) if t.kind == TokenKind::Punctuator(PunctuatorType::Semicolon))
                        {
                            self.next();
                        }
                        break;
                    }
                }
                TokenKind::Punctuator(PunctuatorType::Semicolon) if bracket_stack == 0 => break,

                _ => (),
            }
        }

        Ok(())
    }
}

pub fn byte_range_for_tokens(
    tokens: &[Token],
    start_token: usize,
    end_token: usize,
) -> (usize, usize) {
    let Some(start) = tokens.get(start_token) else {
        return (0, 1);
    };
    let end = tokens
        .get(end_token.saturating_sub(1))
        .map(|token| token.byte_end_index)
        .unwrap_or(start.byte_end_index);

    (
        start.byte_start_index,
        end.max(start.byte_start_index.saturating_add(1)),
    )
}

pub fn file_origin_for_tokens(
    tokens: &[Token],
    start_token: usize,
    end_token: usize,
) -> Option<PathBuf> {
    tokens
        .get(start_token)
        .or_else(|| end_token.checked_sub(1).and_then(|index| tokens.get(index)))
        .and_then(|token| {
            (!token.file_origin.as_os_str().is_empty())
                .then(|| PathBuf::from(token.file_origin.as_ref()))
        })
}

pub fn diagnostic_pointer_for_token(
    default_file: &std::path::Path,
    token: &Token,
    previous_token: Option<&Token>,
) -> DiagnosticPointer {
    let file = file_for_token(default_file, token);
    let anchor_token = previous_token.unwrap_or(token);
    let diagnostic_start = std::fs::read_to_string(&file)
        .ok()
        .map(|source| line_content_start_byte(&source, anchor_token.byte_start_index))
        .unwrap_or(anchor_token.byte_start_index);
    let diagnostic_end = anchor_token
        .byte_end_index
        .max(anchor_token.byte_start_index.saturating_add(1));

    DiagnosticPointer::new(file, token.byte_start_index)
        .with_diagnostic_range(diagnostic_start, diagnostic_end)
}

fn file_for_token(default_file: &std::path::Path, token: &Token) -> PathBuf {
    if token.file_origin.as_os_str().is_empty() {
        default_file.to_owned()
    } else {
        PathBuf::from(token.file_origin.as_ref())
    }
}

fn line_start_byte(contents: &str, index: usize) -> usize {
    let safe_index = index.min(contents.len());
    contents[..safe_index]
        .rfind('\n')
        .map(|idx| idx + 1)
        .unwrap_or(0)
}

fn line_content_start_byte(contents: &str, index: usize) -> usize {
    let line_start = line_start_byte(contents, index);
    let line_end = contents[line_start..]
        .find('\n')
        .map(|offset| line_start + offset)
        .unwrap_or(contents.len());
    let line = &contents[line_start..line_end];
    let first_non_whitespace = line
        .char_indices()
        .find(|(_, ch)| !ch.is_whitespace())
        .map(|(offset, _)| offset)
        .unwrap_or(0);

    line_start + first_non_whitespace
}
