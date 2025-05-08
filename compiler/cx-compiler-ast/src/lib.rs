use std::collections::HashSet;
use cx_data_ast::lex::token::Token;
use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::parser;

pub mod lex;
pub mod parse;
pub mod preprocessor;

pub type PreprocessContents = String;
pub type LexContents = Vec<Token>;
pub type ParseContents = CXAST;

pub fn preprocess(file_contents: &str) -> PreprocessContents {
    preprocessor::preprocess(file_contents)
}

pub fn lex(preprocess_contents: PreprocessContents) -> LexContents {
    lex::generate_tokens(preprocess_contents.as_str())
}

pub fn parse(lex_contents: LexContents) -> Option<ParseContents> {
    let parser_data = parser::ParserData {
        visibility: parser::VisibilityMode::Package,
        toks: parser::TokenIter {
            slice: &lex_contents,
            index: 0,
        },
        type_symbols: HashSet::new(),
    };

    parse::parse_ast(parser_data)
}