use crate::line_lexer::lex_line;
use crate::preprocessor::{generate_lexable_slice, handle_comment, handle_directive};
use cx_lexer_data::token::{Token, TokenKind};
use cx_util::char_iter::CharIter;
use std::collections::HashMap;

pub(crate) struct Lexer<'a> {
    pub(crate) source: &'a str,
    pub(crate) char_iter: CharIter<'a>,
    pub(crate) macros: HashMap<String, Box<[Token]>>,
    pub(crate) tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Lexer {
            source,
            char_iter: CharIter::new(source),
            macros: HashMap::new(),
            tokens: Vec::new(),
        }
    }

    pub(crate) fn lex_source(&mut self) -> Option<()> {
        let source = self.source.to_string();
        
        while self.char_iter.has_next() {
            if let Some(mut lexable_iter) = self.interpret_directive_line() {
                let tokens_in_line = lex_line(&mut lexable_iter, source.clone())?;

                self.tokens.extend(self.expand_macros(tokens_in_line));
            } else {
                break;
            }
        }

        Some(())
    }

    pub(crate) fn independent_lex(&mut self, str: &str) -> Option<Vec<Token>> {
        let mut sublexer = Lexer::new(str);
        std::mem::swap(&mut sublexer.macros, &mut self.macros);
        sublexer.lex_source()?;
        std::mem::swap(&mut sublexer.macros, &mut self.macros);
        Some(sublexer.tokens)
    }

    // returns text that the lexer can lex over, i.e. not comments or preprocessor directives
    fn interpret_directive_line(&mut self) -> Option<CharIter<'_>> {
        loop {
            self.char_iter.skip_whitespace();

            if !self.char_iter.has_next() {
                return None;
            }

            match self.char_iter.peek() {
                Some('#') => {
                    handle_directive(self);
                    continue;
                }

                Some('/') => {
                    if handle_comment(self) {
                        continue;
                    }
                }

                _ => (),
            }

            return generate_lexable_slice(self);
        }
    }

    fn expand_macros(&self, base_tokens: Vec<Token>) -> Vec<Token> {
        base_tokens
            .into_iter()
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
