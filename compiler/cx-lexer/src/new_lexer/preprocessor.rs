use std::{collections::{HashMap, HashSet}, path::PathBuf};

use cx_tokens::token::Token;

pub enum MacroSymbol {
    Definition(Box<[Token]>),
    Function {
        input: Box<[String]>,
        output_template: Box<[Token]>
    }
}

pub struct Preprocessor {
    pub seen_files: HashSet<PathBuf>,
    pub defined_symbols: HashMap<String, MacroSymbol>,
}