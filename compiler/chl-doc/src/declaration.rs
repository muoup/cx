use std::path::Path;

use chl_tokens::token::{PunctuatorType, Token, TokenKind};

// Where a declaration whose header ends at 'from' ends: at its terminating ';', or at the braces that close it
pub fn end(tokens: &[Token], file: &Path, from: usize) -> Option<usize> {
    let in_file = |token: &&Token| *token.file_origin == *file;
    let terminated = tokens
        .iter()
        .filter(in_file)
        .rfind(|token| token.byte_end_index <= from)
        .is_some_and(|token| punctuator(token) == Some(PunctuatorType::Semicolon));
    if terminated {
        return Some(from);
    }

    let mut depth = 0usize;
    let mut closed = None;
    for token in tokens
        .iter()
        .filter(in_file)
        .filter(|token| token.byte_start_index >= from)
    {
        let punctuator = punctuator(token);
        if let Some(end) = closed {
            return Some(match punctuator {
                Some(PunctuatorType::Semicolon) => token.byte_end_index,
                _ => end,
            });
        }
        match punctuator {
            Some(PunctuatorType::OpenParen | PunctuatorType::OpenBracket | PunctuatorType::OpenBrace) => {
                depth += 1
            }
            Some(PunctuatorType::CloseParen | PunctuatorType::CloseBracket) => {
                depth = depth.saturating_sub(1)
            }
            Some(PunctuatorType::CloseBrace) => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    closed = Some(token.byte_end_index);
                }
            }
            Some(PunctuatorType::Semicolon) if depth == 0 => return Some(token.byte_end_index),
            _ => {}
        }
    }
    closed
}

fn punctuator(token: &Token) -> Option<PunctuatorType> {
    match &token.kind {
        TokenKind::Punctuator(punctuator) => Some(*punctuator),
        _ => None,
    }
}

// Lines after the first lose the indentation of the line the declaration starts on
pub fn dedent(source: &str, start: usize, end: usize) -> String {
    let line_start = source[..start].rfind('\n').map_or(0, |index| index + 1);
    let line = &source[line_start..];
    let indent = line.len() - line.trim_start_matches([' ', '\t']).len();
    source[start..end]
        .lines()
        .enumerate()
        .map(|(index, line)| {
            let line = line.trim_end();
            match index {
                0 => line,
                _ => {
                    let leading = line.len() - line.trim_start_matches([' ', '\t']).len();
                    &line[leading.min(indent)..]
                }
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn collapse(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}
