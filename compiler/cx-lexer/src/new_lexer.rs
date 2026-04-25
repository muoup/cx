use std::{collections::{HashMap, HashSet}, path::{Path, PathBuf}};

use cx_tokens::token::Token;

pub mod lexer;
pub mod preprocessor;

pub fn lex_compilation_unit(file: &Path) -> Box<[Token]> {
    todo!()
}
    