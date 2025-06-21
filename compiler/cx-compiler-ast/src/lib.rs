use cx_data_ast::lex::token::{TokenKind, Token};
use cx_data_ast::parse::ast::CXAST;

pub mod lex;
pub mod parse;
pub mod preprocessor;

pub type PreprocessContents = String;
pub type LexContents = Vec<Token>;
pub type ParseContents = CXAST;