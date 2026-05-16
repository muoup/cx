use std::{path::Path, sync::Arc};

use cx_tokens::token::{Token, TokenKind};
use cx_util::{CXResult, char_iter::CharIter};

use crate::{
    context::SourceInput,
    lexer::{comments::handle_comment, source::SourceFrame, token_rules},
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
                    let handled = self.frame.with_iter(handle_comment);
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
            )?;
            return Ok(LexEvent::Tokens(tokens));
        }
    }

    fn next_token_range(&mut self) -> Option<(usize, usize, bool)> {
        self.frame.skip_whitespace();
        let start = self.frame.cursor;

        self.frame.with_iter(|iter| {
            while let Some(c) = iter.next() {
                match c {
                    '\n' => break,
                    '/' => {
                        if iter.peek() == Some('/') || iter.peek() == Some('*') {
                            iter.back();
                            break;
                        }
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

pub(crate) fn tokenize_text(source: &str, file_origin: &Path) -> CXResult<Vec<Token>> {
    tokenize_range(source, 0, source.len(), file_origin)
}

fn tokenize_range(
    source: &str,
    start: usize,
    end: usize,
    file_origin: &Path,
) -> CXResult<Vec<Token>> {
    let mut iter = CharIter::new(&source[..end]);
    iter.current_iter = start;
    let mut accumulator = TokenAccumulator::new(&mut iter, file_origin);
    accumulator.generate_tokens()?;
    Ok(accumulator.tokens)
}

struct TokenAccumulator<'a> {
    iter: &'a mut CharIter<'a>,
    file_origin: Arc<Path>,
    last_consume: usize,
    tokens: Vec<Token>,
}

impl<'a> TokenAccumulator<'a> {
    fn new(iter: &'a mut CharIter<'a>, file_origin: &Path) -> Self {
        Self {
            last_consume: iter.current_iter,
            iter,
            file_origin: Arc::from(file_origin),
            tokens: Vec::new(),
        }
    }

    fn generate_tokens(&mut self) -> CXResult<()> {
        while self.iter.has_next() && self.iter.peek() != Some('\n') {
            if self.last_consume == self.iter.current_iter
                && let Some(token) = token_rules::literal_or_prefixed_token(
                    self.iter,
                    &self.file_origin.to_string_lossy(),
                )?
            {
                self.add_token(token);
                self.last_consume = self.iter.current_iter;
            }

            let previous_lex = self.iter.current_iter;

            if let Some(operator) =
                token_rules::operator(self.iter).or_else(|| token_rules::punctuator(self.iter))
            {
                self.consume(previous_lex);
                self.add_token(operator);
                self.last_consume = self.iter.current_iter;
            } else if Some(true) == self.iter.peek().map(|c| c.is_whitespace()) {
                self.consume(previous_lex);

                while let Some(true) = self.iter.peek().map(|c| c.is_whitespace()) {
                    self.iter.next();
                }

                self.last_consume = self.iter.current_iter;
            } else {
                self.iter.next();
            }
        }

        self.consume(self.iter.current_iter);
        Ok(())
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            byte_start_index: self.last_consume,
            byte_end_index: self.iter.current_iter,
            file_origin: self.file_origin.clone(),
        })
    }

    fn consume(&mut self, up_to: usize) {
        if up_to == self.last_consume {
            return;
        }

        let text = self.iter.source[self.last_consume..up_to].to_string();
        if text.chars().any(|c| !c.is_whitespace()) {
            self.add_token(TokenKind::from_str(text));
        }

        self.last_consume = self.iter.current_iter;
    }
}
