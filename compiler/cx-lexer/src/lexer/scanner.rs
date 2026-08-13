use std::{path::Path, sync::Arc};

use cx_log::CXResult;
use cx_tokens::token::{Token, TokenKind};

use crate::{
    context::SourceInput,
    lexer::{
        comments::handle_comment,
        source::{LanguageMode, LexCursor, SourceFrame},
        token_rules,
    },
};

pub(crate) enum LexEvent {
    Continue,
    Tokens(Vec<Token>),
    Directive,
    Eof,
}

pub(crate) enum LexTransition {
    Continue,
    #[allow(dead_code)]
    MoveTo(usize),
    PushSource(SourceInput),
    PopSource,
}

pub(crate) struct Lexer<'a> {
    frame: &'a mut SourceFrame,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(frame: &'a mut SourceFrame) -> Self {
        Self { frame }
    }

    pub(crate) fn step(&mut self) -> CXResult<LexEvent> {
        loop {
            self.frame.skip_whitespace();

            if !self.frame.has_next() {
                return Ok(LexEvent::Eof);
            }

            match self.frame.peek() {
                Some('#') => return Ok(LexEvent::Directive),
                Some('/') => {
                    let handled = self.frame.with_cursor(handle_comment);
                    if handled {
                        continue;
                    }
                }
                _ => {}
            }

            let Some((start, end, active)) = self.next_token_range() else {
                return Ok(LexEvent::Continue);
            };

            if !active {
                return Ok(LexEvent::Continue);
            }

            let tokens = tokenize_range(
                &self.frame.source,
                start,
                end,
                self.frame.file_path.as_path(),
                self.frame.language_mode,
            )?;
            return Ok(LexEvent::Tokens(tokens));
        }
    }

    fn next_token_range(&mut self) -> Option<(usize, usize, bool)> {
        self.frame.skip_whitespace();
        let start = self.frame.cursor;

        self.frame.with_cursor(|cursor| {
            while let Some(c) = cursor.next() {
                match c {
                    '\n' => break,
                    '/' if (cursor.peek() == Some('/') || cursor.peek() == Some('*')) => {
                        cursor.back();
                        break;
                    }
                    _ => {}
                }
            }
        });

        if start == self.frame.cursor {
            return None;
        }

        Some((start, self.frame.cursor, self.frame.is_active()))
    }
}

pub(crate) fn tokenize_text(
    source: &str,
    file_origin: &Path,
    language_mode: LanguageMode,
) -> CXResult<Vec<Token>> {
    tokenize_range(source, 0, source.len(), file_origin, language_mode)
}

fn tokenize_range(
    source: &str,
    start: usize,
    end: usize,
    file_origin: &Path,
    language_mode: LanguageMode,
) -> CXResult<Vec<Token>> {
    let mut cursor = LexCursor::new(&source[..end], file_origin, start);
    let mut accumulator = TokenAccumulator::new(&mut cursor, file_origin, language_mode);
    accumulator.generate_tokens()?;
    Ok(accumulator.tokens)
}

struct TokenAccumulator<'a> {
    cursor: &'a mut LexCursor<'a>,
    file_origin: Arc<Path>,
    last_consume: usize,
    tokens: Vec<Token>,
    language_mode: LanguageMode,
}

impl<'a> TokenAccumulator<'a> {
    fn new(cursor: &'a mut LexCursor<'a>, file_origin: &Path, language_mode: LanguageMode) -> Self {
        Self {
            last_consume: cursor.cursor(),
            cursor,
            file_origin: Arc::from(file_origin),
            tokens: Vec::new(),
            language_mode,
        }
    }

    fn generate_tokens(&mut self) -> CXResult<()> {
        while self.cursor.has_next() && self.cursor.peek() != Some('\n') {
            if self.last_consume == self.cursor.cursor()
                && let Some(token) = token_rules::literal_or_prefixed_token(self.cursor)?
            {
                self.add_token(token, self.cursor.cursor());
                self.last_consume = self.cursor.cursor();
            }

            let previous_lex = self.cursor.cursor();

            if let Some(operator) =
                token_rules::operator(self.cursor).or_else(|| token_rules::punctuator(self.cursor))
            {
                self.consume(previous_lex);
                self.add_token(operator, self.cursor.cursor());
                self.last_consume = self.cursor.cursor();
            } else if Some(true) == self.cursor.peek().map(|c| c.is_whitespace()) {
                self.consume(previous_lex);

                while let Some(true) = self.cursor.peek().map(|c| c.is_whitespace()) {
                    self.cursor.next();
                }

                self.last_consume = self.cursor.cursor();
            } else {
                self.cursor.next();
            }
        }

        self.consume(self.cursor.cursor());
        Ok(())
    }

    fn add_token(&mut self, kind: TokenKind, byte_end_index: usize) {
        self.tokens.push(Token {
            kind,
            byte_start_index: self.last_consume,
            byte_end_index,
            file_origin: self.file_origin.clone(),
        })
    }

    fn consume(&mut self, up_to: usize) {
        if up_to == self.last_consume {
            return;
        }

        let text = self.cursor.source()[self.last_consume..up_to].to_string();
        if text.chars().any(|c| !c.is_whitespace()) {
            let kind = TokenKind::from_str(text);
            self.add_token(
                match self.language_mode {
                    LanguageMode::C => kind.into_c_mode(),
                    LanguageMode::Cx => kind,
                },
                up_to,
            );
        }

        self.last_consume = up_to;
    }
}
