use cx_data_lexer::token::{Token, TokenKind};
use std::collections::HashMap;
use cx_util::char_iter::CharIter;
use crate::line_lexer::lex_line;
use crate::preprocessor::handle_directive;

pub(crate) struct Lexer<'a> {
    source: &'a str,
    char_iter: CharIter<'a>,
    macros: HashMap<String, Box<[Token]>>
}

impl Lexer<'_> {
    pub(crate) fn new(source: &str) -> Self {
        Lexer {
            source,
            char_iter: CharIter::new(source),

            in_multiline_comment: false,
            macros: HashMap::new()
        }
    }

    pub(crate) fn lex_source(&mut self, source: &str) -> Option<Vec<Token>> {
        let mut tokens = Vec::new();

        while self.char_iter.has_next() {
            let mut lexable_iter = self.interpret_directive_line();

            let tokens_in_line = lex_line(&mut lexable_iter)?;
            self.char_iter.current_iter = lexable_iter.current_iter;
            tokens.extend(self.expand_macros(tokens_in_line));
        }

        Some(tokens)
    }

    // returns text that the lexer can lex over, i.e. not comments or preprocessor directives
    fn interpret_directive_line(&mut self) -> Option<CharIter> {
        loop {
            self.char_iter.skip_whitespace();

            if !self.char_iter.has_next() {
                return None;
            }

            match self.char_iter.peek() {
                Some('#') => {
                    handle_directive(self);
                    continue;
                },

                Some('/') => {
                    self.char_iter.next();

                    match self.char_iter.peek() {
                        Some('/') => {
                            self.char_iter.next();
                            self.char_iter.skip_line();
                            continue;
                        },

                        Some('*') => {
                            self.char_iter.next();

                            while self.char_iter.has_next() {
                                if self.char_iter.peek() == Some('*') {
                                    self.char_iter.next();

                                    if self.char_iter.peek() == Some('/') {
                                        self.char_iter.next();
                                    }
                                } else {
                                    self.char_iter.next();
                                }
                            }

                            continue;
                        },

                        _ => self.char_iter.back(),
                    }
                },

                _ => ()
            }

            todo!("Find either the end of the line, or the beginning of a comment, and return a CharIter over a slice beginning\
                    from the beginning of the Lexer slice to the beginning of the comment or the end of the line.")
        }
    }

    fn expand_macros(&self, base_tokens: Vec<Token>) -> Vec<Token> {
        base_tokens.into_iter()
            .flat_map(|token| {
                if let TokenKind::Identifier(name) = &token.kind {
                    if let Some(macro_body) = self.macros.get(name) {
                        // This is a macro. Expand it.
                        let expansion_start = token.start_index;
                        let expansion_end = token.end_index;
                        let expansion_line = token.line;

                        return macro_body
                            .clone()
                            .into_iter()
                            .map(|mut t| {
                                // Overwrite the location info of each token in the expansion
                                // with the location of the macro identifier itself.
                                t.start_index = expansion_start;
                                t.end_index = expansion_end;
                                t.line = expansion_line;
                                t
                            })
                            .collect::<Vec<_>>();
                    }
                }

                // Not an identifier or not a macro, just return the token as is.
                vec![token]
            })
            .collect::<Vec<_>>()
    }
}