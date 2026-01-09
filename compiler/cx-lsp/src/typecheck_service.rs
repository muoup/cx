//! Typecheck service for LSP integration
//!
//! This module provides utilities for converting compiler type errors
//! into LSP diagnostics format.

use std::collections::HashMap;
use std::path::Path;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};
use cx_pipeline::LSPErrors;
use cx_typechecker::log::TypeError;

/// Convert a TypeError to an LSP Diagnostic
///
/// This function reads the file contents to convert token indices
/// (character offsets) to line/column positions expected by LSP.
pub fn type_error_to_diagnostic(error: &TypeError) -> Diagnostic {
    let file_path = &error.compilation_unit;
    let file_contents = std::fs::read_to_string(file_path).unwrap_or_default();

    // Convert character offsets to line/column (1-based)
    let (start_line, start_col) = cx_log::get_error_loc(&file_contents, error.token_start);
    let (end_line, end_col) = cx_log::get_error_loc(&file_contents, error.token_end);

    Diagnostic {
        range: Range {
            start: Position {
                line: (start_line - 1) as u32,  // LSP uses 0-based line numbers
                character: (start_col - 1) as u32, // LSP uses 0-based columns
            },
            end: Position {
                line: (end_line - 1) as u32,
                character: (end_col - 1) as u32,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        message: error.message.clone(),
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

            // For fatal errors, we may have limited line information
            let line_num = line.unwrap_or(1) as u32;
            let line_count = file_contents.lines().count() as u32;

            Diagnostic {
                range: Range {
                    start: Position {
                        line: line_num.saturating_sub(1),
                        character: 0,
                    },
                    end: Position {
                        line: line_num.min(line_count),
                        character: 1000, // Arbitrary large number to span the line
                    },
                },
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