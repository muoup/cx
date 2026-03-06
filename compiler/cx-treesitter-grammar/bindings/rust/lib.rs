//! Tree-sitter support for the CX language.

use tree_sitter_language::LanguageFn;

unsafe extern "C" {
    fn tree_sitter_cx() -> *const ();
}

/// The tree-sitter language for CX.
pub const LANGUAGE: LanguageFn = unsafe { LanguageFn::from_raw(tree_sitter_cx) };

/// The generated node type definitions for this grammar.
pub const NODE_TYPES: &str = include_str!("../../src/node-types.json");

/// The syntax highlighting query for this grammar.
pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");

#[cfg(test)]
mod tests {
    #[test]
    fn test_can_load_grammar() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&super::LANGUAGE.into())
            .expect("Error loading CX tree-sitter grammar");
    }
}
