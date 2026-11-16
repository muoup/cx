//! Tree-sitter support for the Chloro language.

use tree_sitter_language::LanguageFn;

unsafe extern "C" {
    fn tree_sitter_chl() -> *const ();
}

/// The tree-sitter language for Chloro.
pub const LANGUAGE: LanguageFn = unsafe { LanguageFn::from_raw(tree_sitter_chl) };

/// The generated node type definitions for this grammar.
pub const NODE_TYPES: &str = include_str!("../../src/node-types.json");

/// The syntax highlighting query for this grammar.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");
