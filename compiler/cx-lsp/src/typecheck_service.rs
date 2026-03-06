//! Typecheck service for LSP integration
//!
//! This module provides utilities for converting compiler type errors
//! into LSP diagnostics format.

use std::collections::HashMap;
use std::path::Path;
use cx_tokens::token::Token;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Position, Range, Url,
};
use cx_pipeline::LSPErrors;
use cx_typechecker::log::TypeError;

fn byte_index_to_position(file_contents: &str, index: usize) -> Position {
    let mut remaining = index.min(file_contents.len());

    for (line_num, line) in file_contents.lines().enumerate() {
        let line_len = line.len();
        if remaining <= line_len {
            return Position {
                line: line_num as u32,
                character: line[..remaining].chars().count() as u32,
            };
        }

        remaining = remaining.saturating_sub(line_len + 1);
    }

    Position {
        line: file_contents.lines().count().saturating_sub(1) as u32,
        character: file_contents
            .lines()
            .last()
            .map(|line| line.chars().count() as u32)
            .unwrap_or(0),
    }
}

fn token_range(file_contents: &str, tokens: &[Token], token_start: usize, token_end: usize) -> Range {
    let start_token_index = token_start.min(tokens.len().saturating_sub(1));
    let end_token_index = token_end
        .saturating_sub(1)
        .min(tokens.len().saturating_sub(1));

    let start = tokens
        .get(start_token_index)
        .map(|token| byte_index_to_position(file_contents, token.start_index))
        .unwrap_or_default();
    let end = tokens
        .get(end_token_index)
        .map(|token| byte_index_to_position(file_contents, token.end_index))
        .unwrap_or(start);

    Range { start, end }
}

fn fallback_range(file_contents: &str, line: Option<usize>) -> Range {
    let target_line = line.unwrap_or(1).saturating_sub(1) as u32;
    let last_line = file_contents.lines().count().saturating_sub(1) as u32;
    let line_index = target_line.min(last_line);
    let line_len = file_contents
        .lines()
        .nth(line_index as usize)
        .map(|content| content.chars().count() as u32)
        .unwrap_or(0);

    Range {
        start: Position {
            line: line_index,
            character: 0,
        },
        end: Position {
            line: line_index,
            character: line_len.max(1),
        },
    }
}

fn related_information(uri: &Url, range: Range, notes: &[String]) -> Option<Vec<DiagnosticRelatedInformation>> {
    if notes.is_empty() {
        return None;
    }

    Some(
        notes
            .iter()
            .map(|note| DiagnosticRelatedInformation {
                location: Location {
                    uri: uri.clone(),
                    range,
                },
                message: note.clone(),
            })
            .collect(),
    )
}

pub fn type_error_to_diagnostic(error: &TypeError) -> Diagnostic {
    let file_path = &error.compilation_unit;
    let file_contents = std::fs::read_to_string(file_path).unwrap_or_default();
    let uri = Url::from_file_path(file_path).ok();
    let range = cx_lexer::lex(&file_contents)
        .map(|tokens| token_range(&file_contents, &tokens, error.token_start, error.token_end))
        .unwrap_or_else(|| fallback_range(&file_contents, None));
    let related_information = uri
        .as_ref()
        .and_then(|uri| related_information(uri, range, &error.notes));

    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        message: error.message.clone(),
        related_information,
        source: Some("cx".to_string()),
        ..Default::default()
    }
}

/// Convert an LSPErrors to an LSP Diagnostic
///
/// This handles both TypeError and FatalError variants.
pub fn lsp_error_to_diagnostic(error: &LSPErrors) -> Diagnostic {
    match error {
        LSPErrors::TypeError(e) => type_error_to_diagnostic(e),
        LSPErrors::FatalError { compilation_unit, message, line } => {
            let file_contents = std::fs::read_to_string(compilation_unit).unwrap_or_default();

            Diagnostic {
                range: fallback_range(&file_contents, *line),
                severity: Some(DiagnosticSeverity::ERROR),
                message: message.clone(),
                source: Some("cx".to_string()),
                ..Default::default()
            }
        }
    }
}

/// Group diagnostics by file for publishing
///
/// LSP requires publishing diagnostics separately for each file.
/// This function takes a list of LSPErrors and groups them by file.
pub fn group_diagnostics_by_file(errors: &[LSPErrors]) -> HashMap<Url, Vec<Diagnostic>> {
    let mut grouped = HashMap::new();

    for error in errors {
        let compilation_unit = match error {
            LSPErrors::TypeError(e) => &e.compilation_unit,
            LSPErrors::FatalError { compilation_unit, .. } => compilation_unit,
        };

        let uri = match Url::from_file_path(Path::new(compilation_unit)) {
            Ok(u) => u,
            Err(_) => continue,
        };

        grouped
            .entry(uri)
            .or_insert_with(Vec::new)
            .push(lsp_error_to_diagnostic(error));
    }

    grouped
}
